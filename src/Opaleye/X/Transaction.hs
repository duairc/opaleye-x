{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.X.Transaction
    ( Transaction, transaction

    , Run, one, only, many, some
    , query, count
    , SelectException (SelectParseError, SelectRecordNotFound)
    , Select, selected, selecting, parse, select

    , insert, insertReturning
    , update, updateReturning
    , delete
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, (<|>), empty)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Control.Exception (Exception, SomeException)
import           Control.Monad (MonadPlus, (>=>))
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Bifunctor (bimap)
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
import           Data.Int (Int64)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadInner)
import           Control.Monad.Lift.Base (MonadBase)
import           Monad.Abort (MonadAbort)
import           Monad.Recover (MonadRecover, recover)
import           Monad.Throw (throw)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Aggregate (countRows)
import           Opaleye.Manipulation
                     ( runInsertMany, runInsertManyReturning
                     , runUpdate, runUpdateReturning
                     , runDelete
                     )
import           Opaleye.Order (limit)
import           Opaleye.QueryArr (Query)
import           Opaleye.RunQuery
                     ( QueryRunner, runQueryExplicit, runQueryFoldExplicit
                     )


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Optional (Optionalize, optionalize)
import           Opaleye.X.TF (PGIn, PGOut, PG, pg)
import           Opaleye.X.Table (Table)


-- postgresql-simple ---------------------------------------------------------
import           Database.PostgreSQL.Simple (Connection, withTransaction)
import           Database.PostgreSQL.Simple.Transaction (withSavepoint)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product
                     ( ProductProfunctor, purePP, (****), (***$)
                     )
import           Data.Profunctor.Product (SumProfunctor, (+++!))
import           Data.Profunctor.Product.Default (def)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
(&&&) :: ProductProfunctor p => p a b -> p a c -> p a (b, c)
f &&& g = (,) ***$ f **** g
infixr 3 &&&


------------------------------------------------------------------------------
(***) :: ProductProfunctor p => p a b -> p c d -> p (a, c) (b, d)
f *** g = dimap fst (,) f **** lmap snd g
infixr 3 ***


------------------------------------------------------------------------------
(+++) :: SumProfunctor p => p a b -> p c d -> p (Either a c) (Either b d)
(+++) = (+++!)
infixr 2 +++


------------------------------------------------------------------------------
transaction :: Transaction a -> Connection -> IO a
transaction (Transaction (ReaderT f)) c = withTransaction c (f c)


------------------------------------------------------------------------------
newtype Transaction a = Transaction (ReaderT Connection IO a)
  deriving
    ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix
    , Typeable, Generic, Generic1, MonadAbort SomeException, MonadIO
    , MonadInner IO
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )


------------------------------------------------------------------------------
instance MonadRecover SomeException Transaction where
    recover (Transaction (ReaderT f)) handler = Transaction $ ReaderT $ \c ->
        withSavepoint c (f c) `recover` \e ->
            let Transaction (ReaderT g) = handler e in g c


------------------------------------------------------------------------------
instance Semigroup (Transaction a) where
    (<>) = (<|>)


------------------------------------------------------------------------------
instance Monoid (Transaction a) where
    mempty = empty
    mappend = (<|>)


------------------------------------------------------------------------------
instance MonadBase Transaction Transaction


------------------------------------------------------------------------------
type Run f = forall p a. QueryRunner p a -> Query p -> Transaction (f a)


------------------------------------------------------------------------------
one :: Run Maybe
one runner q = Transaction . ReaderT $ \c ->
    runQueryFoldExplicit runner c (limit 1 q) Nothing (const (pure . Just))


------------------------------------------------------------------------------
only :: Run Identity
only runner q = do
    result <- one runner q
    case result of
        Nothing -> throw SelectRecordNotFound
        Just a -> pure $ Identity a


------------------------------------------------------------------------------
many :: Run []
many runner = Transaction . ReaderT . flip (runQueryExplicit runner)


------------------------------------------------------------------------------
some :: Run NonEmpty
some runner q = do
    result <- many runner q
    case nonEmpty result of
        Nothing -> throw SelectRecordNotFound
        Just a -> pure a


------------------------------------------------------------------------------
query :: PGOut p a => Run f -> Query p -> Transaction (f a)
query run = run def


------------------------------------------------------------------------------
count :: Query p -> Transaction Int64
count = fmap runIdentity . only def . countRows


------------------------------------------------------------------------------
data SelectException = SelectParseError String | SelectRecordNotFound
  deriving (Eq, Ord, Read, Show, Generic, Typeable)
instance Exception SelectException


------------------------------------------------------------------------------
data Select p a = forall q b.
    Select !(p -> q) !(QueryRunner q b) !(b -> Either String a)


------------------------------------------------------------------------------
instance Profunctor Select where
    dimap f g (Select l q r) = Select (l . f) q (fmap g . r)


------------------------------------------------------------------------------
instance ProductProfunctor Select where
    purePP = Select id (purePP ()) . const . pure
    Select lf qf rf **** Select la qa ra = Select (lf &&& la) (qf *** qa) go
      where
        go (f, a) = rf f <*> ra a


------------------------------------------------------------------------------
instance SumProfunctor Select where
    Select lf qf rf +++! Select la qa ra = Select (bimap lf la) (qf +++ qa) go
      where
        go = rmap (either (fmap Left) (fmap Right)) $ rf +++ ra


------------------------------------------------------------------------------
instance Functor (Select a) where
    fmap = rmap


------------------------------------------------------------------------------
instance Applicative (Select a) where
    pure = purePP
    (<*>) = (****)


------------------------------------------------------------------------------
selected :: PGOut p a => Select p a
selected = selecting def


------------------------------------------------------------------------------
selecting :: QueryRunner p a -> Select p a
selecting runner = Select id runner pure


------------------------------------------------------------------------------
parse :: (a -> Either String b) -> Select p a -> Select p b
parse f (Select l q r) = Select l q (r >=> f)


------------------------------------------------------------------------------
select :: Traversable f => Run f -> Select p a -> Query p -> Transaction (f a)
select run (Select projection runner parser) q = do
    result <- traverse parser <$> run runner (rmap projection q)
    case result of
        Left message -> throw (SelectParseError message)
        Right as -> pure as


------------------------------------------------------------------------------
insert :: PGIn as ws => Table ws -> [as] -> Transaction Int64
insert t as = Transaction . ReaderT $ \c -> runInsertMany c t (map pg as)


------------------------------------------------------------------------------
insertReturning :: (Optionalize rs ws, PGIn as ws, PGOut p a)
    => Table ws -> (rs -> p) -> [as] -> Transaction [a]
insertReturning t f as = Transaction . ReaderT $ \c ->
    runInsertManyReturning c t (map pg as) f


------------------------------------------------------------------------------
update :: Optionalize rs ws
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> Transaction Int64
update t f p = Transaction . ReaderT $ \c ->
    runUpdate c t (f . optionalize) p


------------------------------------------------------------------------------
updateReturning :: (Optionalize rs ws, PGOut p a)
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> (rs -> p)
    -> Transaction [a]
updateReturning t f p g = Transaction . ReaderT $ \c ->
    runUpdateReturning c t (f . optionalize) p g


------------------------------------------------------------------------------
delete
    :: Optionalize rs ws => Table ws
    -> (rs -> PG Bool)
    -> Transaction Int64
delete t f = Transaction . ReaderT $ \c -> runDelete c t f
