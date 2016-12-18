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

    , LiftRecord
    , liftRecord
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record)
import           Data.Labeled (Labeled (Labeled))


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
import           Opaleye.Table (Table)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.TF (PG, ToPG, FromPG, pg)


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
insert :: ToPG a w => Table w r -> [a] -> Transaction Int64
insert t as = Transaction . ReaderT $ \c -> runInsertMany c t (map pg as)


------------------------------------------------------------------------------
insertReturning :: (ToPG a w, FromPG b' b)
    => Table w r
    -> (r -> b')
    -> [a]
    -> Transaction [b]
insertReturning t f as = Transaction . ReaderT $ \c ->
    runInsertManyReturning c t (map pg as) f


------------------------------------------------------------------------------
update :: (LiftRecord r' w', w ~ Record w', r ~ Record r')
    => Table w r -> (w -> w) -> (r -> PG Bool) -> Transaction Int64
update t f p = Transaction . ReaderT $ \c -> runUpdate c t (f . liftRecord) p


------------------------------------------------------------------------------
updateReturning
    :: (FromPG a' a, LiftRecord r' w', w ~ Record w', r ~ Record r')
    => Table w r -> (w -> w) -> (r -> PG Bool) -> (r -> a') -> Transaction [a]
updateReturning t f p g = Transaction . ReaderT $ \c ->
    runUpdateReturning c t (f . liftRecord) p g


------------------------------------------------------------------------------
delete :: Table w r -> (r -> PG Bool) -> Transaction Int64
delete t f = Transaction . ReaderT $ \c -> runDelete c t f


------------------------------------------------------------------------------
class LiftRecord r w where
    liftRecord :: Functor f => Product (Labeled f) r -> Product (Labeled f) w


------------------------------------------------------------------------------
instance LiftRecord '[] '[] where
    liftRecord Nil = Nil


------------------------------------------------------------------------------
instance LiftRecord as bs => LiftRecord (a ': as) (a ': bs) where
    liftRecord (Cons a as) = Cons a (liftRecord as)


------------------------------------------------------------------------------
instance (Applicative f, LiftRecord as bs) =>
    LiftRecord ('(s, a) ': as) ('(s, f a) ': bs)
  where
    liftRecord (Cons (Labeled a) as) =
        Cons (Labeled (pure <$> a)) (liftRecord as)
