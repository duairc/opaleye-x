{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
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
    , Select, selected, selecting, parse, select

    , insert, insertReturning
    , update, updateReturning
    , delete

    , getCurrentTime, nextUUID, nextRandom
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, (<|>), empty)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Control.Exception
                     ( Exception
                     , PatternMatchFail (PatternMatchFail)
                     , SomeException
                     )
import           Control.Monad (MonadPlus, mzero, mplus, (>=>))
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as F (fail)
#endif
import           Control.Monad.Fix (MonadFix)
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
import           Control.Monad.Lift (lift)
import           Control.Monad.Lift.Base (MonadBase)
import           Control.Monad.Lift.IO (MonadIO, liftIO)
import           Monad.Abort (MonadAbort)
import           Monad.Catch (catch, try)
import           Monad.Mask (mask)
import           Monad.Reader (MonadReader, ask)
import           Monad.Recover (MonadRecover, recover)
import           Monad.Throw (throw)
import           Monad.Try (onException)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Aggregate (countRows)
import           Opaleye.Manipulation
                     ( runInsertMany
                     , runInsertManyReturning
                     , runUpdate
                     , runUpdateReturning
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
import           Database.PostgreSQL.Simple
                     ( Connection
                     , SqlError
                     , withTransaction
                     )
import           Database.PostgreSQL.Simple.Transaction
                     ( isFailedTransactionError
                     , newSavepoint
                     , releaseSavepoint
                     , rollbackToAndReleaseSavepoint
                     )


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, Star (Star))
import           Data.Profunctor.Composition (Procompose (Procompose))


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, purePP, (****))
import           Data.Profunctor.Product.Default (def)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as T (getCurrentTime)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))


-- uuid ----------------------------------------------------------------------
import           Data.UUID (UUID)
import qualified Data.UUID.V1 as U (nextUUID)
import qualified Data.UUID.V4 as U (nextRandom)


------------------------------------------------------------------------------
data Zero = Zero
  deriving (Show, Typeable, Generic)
instance Exception Zero


------------------------------------------------------------------------------
transaction :: (MonadReader Connection m, MonadIO m) => Transaction a -> m a
transaction (Transaction (ReaderT f)) = ask >>= \connection -> do
    let ExceptT e = f connection
    liftIO (withTransaction connection (e >>= either throw return))


------------------------------------------------------------------------------
newtype Transaction a =
    Transaction (ReaderT Connection (ExceptT SomeException IO) a)
  deriving
    ( Functor, Applicative, MonadFix, Typeable, Generic, Generic1
    , MonadAbort SomeException
    )


------------------------------------------------------------------------------
instance Monad Transaction where
    return = pure
    Transaction m >>= f = let f' a = let Transaction b = f a in b in
        Transaction $ m >>= f'
    fail = throw . PatternMatchFail


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance MonadFail Transaction where
    fail = throw . PatternMatchFail


#endif
------------------------------------------------------------------------------
instance Alternative Transaction where
    empty = throw Zero
    a <|> b = a `recover` (\e -> b `catch` (\Zero -> throw e))


------------------------------------------------------------------------------
instance MonadPlus Transaction where
    mzero = empty
    mplus = (<|>)


------------------------------------------------------------------------------
instance MonadRecover SomeException Transaction where
    recover (Transaction (ReaderT m)) handler =
        Transaction $ ReaderT $ \connection -> ExceptT $ mask $ \restore -> do
            savepoint <- newSavepoint connection
            e <- restore (let ExceptT m' = m connection in m') `onException`
                rollbackToAndReleaseSavepoint connection savepoint
            case e of
                Left x -> do
                    rollbackToAndReleaseSavepoint connection savepoint
                    let Transaction (ReaderT m') = handler x
                    let ExceptT m'' = m' connection
                    m''
                Right a -> do
                    releaseSavepoint connection savepoint `catch` \x ->
                        if isFailedTransactionError x
                            then do
                                rollbackToAndReleaseSavepoint
                                    connection
                                    savepoint
                            else throw x
                    pure (Right a)


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
liftE :: IO a -> ExceptT SomeException IO a
liftE e = lift (try' e) >>= either throw return
  where
    try' :: IO a -> IO (Either SqlError a)
    try' = try


------------------------------------------------------------------------------
type Run f = forall p a. QueryRunner p a -> Query p -> Transaction (f a)


------------------------------------------------------------------------------
one :: Run Maybe
one runner q = Transaction . ReaderT $ \connection -> liftE $
    runQueryFoldExplicit runner connection (limit 1 q) Nothing
        (const (pure . Just))


------------------------------------------------------------------------------
only :: Run Identity
only runner q = do
    result <- one runner q
    case result of
        Nothing -> throw SelectRecordNotFound
        Just a -> pure $ Identity a


------------------------------------------------------------------------------
many :: Run []
many runner q = Transaction . ReaderT $ \connection -> liftE $
    runQueryExplicit runner connection q


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
newtype Select p a =
    Select (Procompose (Star (Either String)) QueryRunner p a)
  deriving (Functor, Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Applicative (Select a) where
    pure = purePP
    (<*>) = (****)


------------------------------------------------------------------------------
selected :: PGOut p a => Select p a
selected = selecting def


------------------------------------------------------------------------------
selecting :: QueryRunner p a -> Select p a
selecting = Select . Procompose (Star Right)


------------------------------------------------------------------------------
parse :: (a -> Either String b) -> Select p a -> Select p b
parse f (Select (Procompose (Star p) q)) =
    Select (Procompose (Star (p >=> f)) q)


------------------------------------------------------------------------------
select :: Traversable f => Run f -> Select p a -> Query p -> Transaction (f a)
select run (Select (Procompose (Star parser) runner)) q = do
    result <- traverse parser <$> run runner q
    case result of
        Left message -> throw (SelectParseError message)
        Right as -> pure as


------------------------------------------------------------------------------
insert :: PGIn as ws => Table ws -> [as] -> Transaction Int64
insert t as = Transaction . ReaderT $ \c ->
    liftE $ runInsertMany c t (map pg as)


------------------------------------------------------------------------------
insertReturning :: (Optionalize rs ws, PGIn as ws, PGOut p a)
    => Table ws -> (rs -> p) -> [as] -> Transaction [a]
insertReturning t f as = Transaction . ReaderT $ \c -> liftE $
    runInsertManyReturning c t (map pg as) f


------------------------------------------------------------------------------
update :: Optionalize rs ws
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> Transaction Int64
update t f p = Transaction . ReaderT $ \c -> liftE $
    runUpdate c t (f . optionalize) p


------------------------------------------------------------------------------
updateReturning :: (Optionalize rs ws, PGOut p a)
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> (rs -> p)
    -> Transaction [a]
updateReturning t f p g = Transaction . ReaderT $ \c -> liftE $
    runUpdateReturning c t (f . optionalize) p g


------------------------------------------------------------------------------
delete
    :: Optionalize rs ws => Table ws
    -> (rs -> PG Bool)
    -> Transaction Int64
delete t f = Transaction . ReaderT $ \c -> liftE $ runDelete c t f


------------------------------------------------------------------------------
getCurrentTime :: Transaction UTCTime
getCurrentTime = Transaction $ lift $ lift T.getCurrentTime


------------------------------------------------------------------------------
nextUUID :: Transaction (Maybe UUID)
nextUUID = Transaction $ lift $ lift U.nextUUID


------------------------------------------------------------------------------
nextRandom :: Transaction UUID
nextRandom = Transaction $ lift $ lift U.nextRandom
