{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.X.Array
    ( PGArray, PGNil, pgNil, PGSingleton, pgSingleton
    , PGFromList, pgFromList
    , ArrayAgg, arrayAgg
    )
where

-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Aggregate (Aggregator)
import qualified Opaleye.Aggregate as O (arrayAgg)
import           Opaleye.Column (Column)
import           Opaleye.Constant (Constant)
import           Opaleye.Operators (emptyArray, singletonArray)
import           Opaleye.PGTypes (IsSqlType, pgArray)
import qualified Opaleye.PGTypes as O (PGArray)
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Internal


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
-- newtype PGArray a = PGArray (DistributePGArray a)
deriving instance Generic (PGArray a)
deriving instance Typeable PGArray


------------------------------------------------------------------------------
instance (PGArrays a as, PGArrays b bs, Default p as bs, Profunctor p) =>
    Default p (PGArray a) (PGArray b)
  where
    def = dimap (\(PGArray a) -> a) PGArray def


------------------------------------------------------------------------------
instance (Default Constant [a] (Column (O.PGArray p))) =>
    Default (L [] Constant) a (Column (O.PGArray p))
  where
    def = L def


------------------------------------------------------------------------------
instance (PGArrays p ps, Default (L [] Constant) a ps) =>
    Default Constant [a] (PGArray p)
  where
    def = let L p = def in rmap PGArray p


------------------------------------------------------------------------------
instance (Default QueryRunner (Column (O.PGArray p)) [a]) =>
    Default (R [] QueryRunner) (Column (O.PGArray p)) a
  where
    def = R def


------------------------------------------------------------------------------
instance (PGArrays p ps, Default (R [] QueryRunner) ps a) =>
    Default QueryRunner (PGArray p) [a]
  where
    def = let R p = def in lmap (\(PGArray a) -> a) p


------------------------------------------------------------------------------
instance IsSqlType a => Default (L [] (->)) (Column a) (Column (O.PGArray a))
  where
    def = L (pgArray id)


------------------------------------------------------------------------------
newtype NilPP a b = NilPP b


------------------------------------------------------------------------------
instance Profunctor NilPP where
    dimap _ r (NilPP p) = NilPP (r p)


------------------------------------------------------------------------------
instance ProductProfunctor NilPP where
    empty = NilPP ()
    NilPP a ***! NilPP b = NilPP (a, b)


------------------------------------------------------------------------------
instance IsSqlType a =>
    Default NilPP (Column (O.PGArray a)) (Column (O.PGArray a))
  where
    def = NilPP emptyArray


------------------------------------------------------------------------------
type PGNil a =
    ( PGArrays a (DistributePGArray a)
    , Default NilPP (DistributePGArray a) (DistributePGArray a)
    )


------------------------------------------------------------------------------
pgNil :: forall a. PGNil a => PGArray a
pgNil = let NilPP n = p in PGArray n
  where
    p = def :: NilPP (DistributePGArray a) (DistributePGArray a)


------------------------------------------------------------------------------
newtype SingletonPP a b = SingletonPP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance IsSqlType a => Default SingletonPP (Column a) (Column (O.PGArray a))
  where
    def = SingletonPP singletonArray


------------------------------------------------------------------------------
instance Default SingletonPP (Column (O.PGArray a)) (Column (O.PGArray a))
  where
    def = SingletonPP id


------------------------------------------------------------------------------
type PGSingleton a =
    ( PGArrays a (DistributePGArray a)
    , Default SingletonPP a (DistributePGArray a)
    )


------------------------------------------------------------------------------
pgSingleton :: PGSingleton a => a -> PGArray a
pgSingleton = let SingletonPP f = def in PGArray . f


------------------------------------------------------------------------------
type PGFromList a =
    ( PGArrays a (DistributePGArray a)
    , Default (L [] (->)) a (DistributePGArray a)
    )


------------------------------------------------------------------------------
pgFromList :: PGFromList a => [a] -> PGArray a
pgFromList = let L f = def in PGArray . f


------------------------------------------------------------------------------
newtype ArrayAggPP a b = ArrayAggPP (Aggregator a b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default ArrayAggPP (Column a) (Column (O.PGArray a)) where
    def = ArrayAggPP O.arrayAgg


------------------------------------------------------------------------------
type ArrayAgg a =
    ( PGArrays a (DistributePGArray a)
    , Default ArrayAggPP a (DistributePGArray a)
    )


------------------------------------------------------------------------------
arrayAgg :: ArrayAgg a => Aggregator a (PGArray a)
arrayAgg = let ArrayAggPP p = def in rmap PGArray p
