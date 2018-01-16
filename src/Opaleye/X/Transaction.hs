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
    , insert, insertReturning
    , update, updateReturning
    , delete
    , RecordNotFound (RecordNotFound)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, (<|>), empty)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Control.Exception (Exception, SomeException)
import           Control.Monad (MonadPlus)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
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


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (def)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
data RecordNotFound = RecordNotFound
  deriving (Eq, Ord, Read, Show, Generic, Typeable)
instance Exception RecordNotFound


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
        Nothing -> throw RecordNotFound
        Just a -> pure $ Identity a


------------------------------------------------------------------------------
many :: Run []
many runner = Transaction . ReaderT . flip (runQueryExplicit runner)


------------------------------------------------------------------------------
some :: Run NonEmpty
some runner q = do
    result <- many runner q
    case nonEmpty result of
        Nothing -> throw RecordNotFound
        Just a -> pure a


------------------------------------------------------------------------------
query :: PGOut p a => Run f -> Query p -> Transaction (f a)
query run = run def


------------------------------------------------------------------------------
count :: Query p -> Transaction Int64
count = fmap runIdentity . only def . countRows


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
