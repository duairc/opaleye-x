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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.StreamBeneathTheHills.Transaction
    ( Transaction
    , transaction

    , query
    , queryFirst

    , insert
    , insertReturning

    , update
    , updateReturning

    , delete

    , getCurrentTime
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Record)


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
import           Control.Monad (MonadPlus, mzero, mplus)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as F (fail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Data.Maybe (listToMaybe)
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
import           Monad.Reader (MonadReader, ask)
import           Monad.Recover (MonadRecover, recover)
import           Monad.Throw (throw)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Manipulation
                     ( runInsertMany
                     , runInsertManyReturning
                     , runUpdate
                     , runUpdateReturning
                     , runDelete
                     )
import           Opaleye.Order (limit)
import           Opaleye.QueryArr (QueryArr)
import           Opaleye.RunQuery (runQuery)
import qualified Opaleye.Table as O (Table)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.TF
                     ( PGIn
                     , PGOut
                     , PG
                     , pg
                     )
import           Opaleye.StreamBeneathTheHills.Table
                     ( Table
                     , TableSpec
                     , optionify
                     )


-- postgresql-simple ---------------------------------------------------------
import           Database.PostgreSQL.Simple
                     ( Connection
                     , SqlError
                     , withTransaction
                     )


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (lmap)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as T (getCurrentTime)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))


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
    , MonadAbort SomeException, MonadRecover SomeException
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
query :: (PGIn a a', PGOut b' b)  => a -> QueryArr a' b' -> Transaction [b]
query a q = Transaction . ReaderT $ \c -> liftE $
    runQuery c $ lmap (const (pg a)) q


------------------------------------------------------------------------------
queryFirst :: (PGIn a a', PGOut b' b)
    => a -> QueryArr a' b' -> Transaction (Maybe b)
queryFirst a = fmap listToMaybe . query () . limit 1 . lmap (const (pg a))


------------------------------------------------------------------------------
insert :: TableSpec ws rs as bs => Table as -> [Record as] -> Transaction Int64
insert t as = Transaction . ReaderT $ \c ->
    liftE $ runInsertMany c t (map pg as)


------------------------------------------------------------------------------
insertReturning :: (TableSpec ws rs as bs, PGOut p a)
    => Table as
    -> (Record rs -> p)
    -> [Record as]
    -> Transaction [a]
insertReturning t f as = Transaction . ReaderT $ \c -> liftE $
    runInsertManyReturning c t (map pg as) f


------------------------------------------------------------------------------
update :: TableSpec ws rs as bs
    => Table as
    -> (Record ws -> Record ws)
    -> (Record rs -> PG Bool)
    -> Transaction Int64
update t f p = Transaction . ReaderT $ \c -> liftE $
    runUpdate c t (f . optionify) p


------------------------------------------------------------------------------
updateReturning :: (TableSpec ws rs as bs, PGOut p a)
    => Table as
    -> (Record ws -> Record ws)
    -> (Record rs -> PG Bool)
    -> (Record rs -> p)
    -> Transaction [a]
updateReturning t f p g = Transaction . ReaderT $ \c -> liftE $
    runUpdateReturning c t (f . optionify) p g


------------------------------------------------------------------------------
delete
    :: O.Table ws rs
    -> (rs -> PG Bool)
    -> Transaction Int64
delete t f = Transaction . ReaderT $ \c -> liftE $ runDelete c t f


------------------------------------------------------------------------------
getCurrentTime :: Transaction UTCTime
getCurrentTime = Transaction $ lift $ lift $ T.getCurrentTime
