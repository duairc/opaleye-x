{-# LANGUAGE CPP #-}
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
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Record)


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, (<$>), pure)
#endif
import           Control.Monad.Fix (MonadFix)
import           Data.Maybe (listToMaybe)
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Base (MonadBase)
import           Control.Monad.Lift.IO (MonadIO, liftIO)
import           Monad.Reader (MonadReader, ask)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Manipulation
                     ( runInsertMany
                     , runInsertManyReturning
                     , runUpdate
                     , runUpdateReturning
                     , runDelete
                     )
import           Opaleye.Order (limit)
import           Opaleye.QueryArr (Query)
import           Opaleye.RunQuery (runQuery)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.TF (PG, FromPG, pg)
import           Opaleye.StreamBeneathTheHills.Table
                     ( Table
                     , TableArgs
                     , optionify
                     )


-- postgresql-simple ---------------------------------------------------------
import           Database.PostgreSQL.Simple (Connection, withTransaction)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
transaction :: (MonadReader Connection m, MonadIO m) => Transaction a -> m a
transaction (Transaction (ReaderT f)) = ask >>= \connection -> do
    liftIO $ withTransaction connection (f connection)


------------------------------------------------------------------------------
newtype Transaction a = Transaction (ReaderT Connection IO a)
  deriving
    (Functor, Applicative, Monad, MonadFix, Typeable, Generic, Generic1)


------------------------------------------------------------------------------
instance MonadBase Transaction Transaction


------------------------------------------------------------------------------
query :: FromPG a' a => Query a' -> Transaction [a]
query = Transaction . ReaderT . flip runQuery


------------------------------------------------------------------------------
queryFirst :: FromPG a' a => Query a' -> Transaction (Maybe a)
queryFirst = fmap listToMaybe . query . limit 1


------------------------------------------------------------------------------
insert :: TableArgs ws rs as => Table as -> [Record as] -> Transaction Int64
insert t as = Transaction . ReaderT $ \c -> runInsertMany c t (map pg as)


------------------------------------------------------------------------------
insertReturning :: (TableArgs ws rs as, FromPG p a)
    => Table as
    -> (Record rs -> p)
    -> [Record as]
    -> Transaction [a]
insertReturning t f as = Transaction . ReaderT $ \c ->
    runInsertManyReturning c t (map pg as) f


------------------------------------------------------------------------------
update :: TableArgs ws rs as
    => Table as
    -> (Record ws -> Record ws)
    -> (Record rs -> PG Bool)
    -> Transaction Int64
update t f p = Transaction . ReaderT $ \c -> runUpdate c t (f . optionify) p


------------------------------------------------------------------------------
updateReturning :: (TableArgs ws rs as, FromPG p a)
    => Table as
    -> (Record ws -> Record ws)
    -> (Record rs -> PG Bool)
    -> (Record rs -> p)
    -> Transaction [a]
updateReturning t f p g = Transaction . ReaderT $ \c ->
    runUpdateReturning c t (f . optionify) p g


------------------------------------------------------------------------------
delete :: TableArgs ws rs as
    => Table as
    -> (Record rs -> PG Bool)
    -> Transaction Int64
delete t f = Transaction . ReaderT $ \c -> runDelete c t f
