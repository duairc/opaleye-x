{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.X.Transaction
    ( TransactionT, Transaction, transaction
    , Run, one, only, many, some
    , query, count
    , insert, insertReturning
    , update, updateReturning
    , delete
    , RecordNotFound (RecordNotFound)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Control.Applicative (liftA2)
import           Control.Exception (Exception, SomeException)
import           Control.Monad (MonadPlus, unless)
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
import           Control.Monad.Lift
                     ( MonadTrans, MInvariant, MFunctor
                     , MonadTransControl, LayerResult, LayerState
                     , suspend, resume, capture, extract
                     , MonadInner, liftI
                     )
import           Monad.Catch (MonadCatch, catch, handle)
import           Monad.Mask (MonadMask, mask)
import           Monad.Recover (MonadRecover, onException, recover)
import           Monad.ST (MonadST, newRef, readRef, writeRef)
import           Monad.Throw (MonadThrow, throw)
import           Monad.Try (MonadTry, finally)


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
import           Database.PostgreSQL.Simple (Connection)
import           Database.PostgreSQL.Simple.Transaction
                     ( beginMode, defaultTransactionMode, rollback, commit
                     , newSavepoint, releaseSavepoint
                     , rollbackToAndReleaseSavepoint, isFailedTransactionError
                     )


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (def)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
data RecordNotFound = RecordNotFound
  deriving (Eq, Ord, Read, Show, Generic, Typeable)
instance Exception RecordNotFound


------------------------------------------------------------------------------
bracket :: (MonadST v m, MonadMask m, MonadTry m, MonadCatch m)
    => m a -> (a -> m ()) -> (a -> m ()) -> (a -> m b) -> m b
bracket acquire onFailure onSuccess run = mask $ \unmask -> do
    resource <- acquire
    ref <- newRef False
    unmask (run resource)
        `onException` do
            onFailure resource `finally` writeRef ref True
        `finally` do
            finished <- readRef ref
            unless finished $ liftI $ onSuccess resource


------------------------------------------------------------------------------
transaction
    :: (MonadInner IO m, MonadST v m, MonadMask m, MonadTry m, MonadCatch m)
    => TransactionT m a -> Connection -> m a
transaction (TransactionT (ReaderT f)) connection =
    bracket (liftI acquire) (liftI . rollback_) (liftI . commit) f
  where
    acquire = beginMode defaultTransactionMode connection >> pure connection
    rollback_ = handle (\(_ :: IOError) -> return ()) . rollback


------------------------------------------------------------------------------
newtype TransactionT m a = TransactionT (ReaderT Connection m a)
  deriving
    ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix
    , Typeable, Generic, Generic1, MonadIO
    , MonadTrans, MInvariant, MFunctor
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )


------------------------------------------------------------------------------
instance MonadTransControl TransactionT where
    suspend (TransactionT m) = suspend m
    resume = TransactionT . resume
    capture = TransactionT capture
    extract _ (Identity a) = Just a


------------------------------------------------------------------------------
type instance LayerResult TransactionT = LayerResult (ReaderT Connection)
type instance LayerState TransactionT m = LayerState (ReaderT Connection) m


------------------------------------------------------------------------------
instance
    ( MonadInner IO m, MonadST v m, MonadMask m, MonadCatch m, MonadTry m
    )
  =>
    MonadRecover SomeException (TransactionT m)
  where
    recover (TransactionT (ReaderT f)) handler = TransactionT $ ReaderT go
      where
        go connection = run `recover` handler'
          where
            run = bracket acquire onFailure onSuccess (const (f connection))
            acquire = liftI $ newSavepoint connection
            onFailure = liftI . rollbackToAndReleaseSavepoint connection
            onSuccess savepoint = liftI $ do
                releaseSavepoint connection savepoint `catch` \e ->
                    if isFailedTransactionError e
                        then
                            rollbackToAndReleaseSavepoint connection savepoint
                        else throw e
            handler' e = g connection
              where
                TransactionT (ReaderT g) = handler e


------------------------------------------------------------------------------
instance (Applicative m, Semigroup a) => Semigroup (TransactionT m a) where
    (<>) = liftA2 (<>)


------------------------------------------------------------------------------
instance (Applicative m, Monoid a) => Monoid (TransactionT m a) where
    mempty = pure mempty
    mappend = liftA2 mappend


------------------------------------------------------------------------------
type Transaction = TransactionT IO


------------------------------------------------------------------------------
type Run m f = forall p a. QueryRunner p a -> Query p -> TransactionT m (f a)


------------------------------------------------------------------------------
one :: MonadInner IO m => Run m Maybe
one runner q = TransactionT . ReaderT $ \c -> liftI $
    runQueryFoldExplicit runner c (limit 1 q) Nothing (const (pure . Just))


------------------------------------------------------------------------------
only :: (MonadInner IO m, MonadThrow m) => Run m Identity
only runner q = do
    result <- one runner q
    case result of
        Nothing -> throw RecordNotFound
        Just a -> pure $ Identity a


------------------------------------------------------------------------------
many :: MonadInner IO m => Run m []
many runner = TransactionT . ReaderT
    . flip ((liftI .) . runQueryExplicit runner)


------------------------------------------------------------------------------
some :: (MonadInner IO m, MonadThrow m) => Run m NonEmpty
some runner q = do
    result <- many runner q
    case nonEmpty result of
        Nothing -> throw RecordNotFound
        Just a -> pure a


------------------------------------------------------------------------------
query :: PGOut p a => Run m f -> Query p -> TransactionT m (f a)
query run = run def


------------------------------------------------------------------------------
count :: (MonadInner IO m, MonadThrow m) => Query p -> TransactionT m Int64
count = fmap runIdentity . only def . countRows


------------------------------------------------------------------------------
insert :: (MonadInner IO m, PGIn as ws)
    => Table ws -> [as] -> TransactionT m Int64
insert table as = TransactionT . ReaderT $ \connection ->
    liftI $ runInsertMany connection table (map pg as)


------------------------------------------------------------------------------
insertReturning :: (MonadInner IO m, Optionalize rs ws, PGIn as ws, PGOut p a)
    => Table ws -> (rs -> p) -> [as] -> TransactionT m [a]
insertReturning table f as = TransactionT . ReaderT $ \connection ->
    liftI $ runInsertManyReturning connection table (map pg as) f


------------------------------------------------------------------------------
update :: (MonadInner IO m, Optionalize rs ws)
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> TransactionT m Int64
update table f p = TransactionT . ReaderT $ \connection ->
    liftI $ runUpdate connection table (f . optionalize) p


------------------------------------------------------------------------------
updateReturning :: (MonadInner IO m, Optionalize rs ws, PGOut p a)
    => Table ws
    -> (ws -> ws)
    -> (rs -> PG Bool)
    -> (rs -> p)
    -> TransactionT m [a]
updateReturning table f p g = TransactionT . ReaderT $ \connection ->
    liftI $ runUpdateReturning connection table (f . optionalize) p g


------------------------------------------------------------------------------
delete :: (MonadInner IO m, Optionalize rs ws)
    => Table ws
    -> (rs -> PG Bool)
    -> TransactionT m Int64
delete table f = TransactionT . ReaderT $ \connection ->
    liftI $ runDelete connection table f
