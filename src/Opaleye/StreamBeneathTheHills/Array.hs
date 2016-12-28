{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.StreamBeneathTheHills.Array
    ( PGArray
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
import           Opaleye.PGTypes (IsSqlType, pgArray)
import qualified Opaleye.PGTypes as O (PGArray)
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-of-the-stream-beheath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Internal


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor)
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
instance (Columns a p, Default Constant [a] (Column (O.PGArray p))) =>
    Default (L [] Constant) a (Column (O.PGArray p))
  where
    def = L def


------------------------------------------------------------------------------
instance (Columns a p, PGArrays p ps, Default (L [] Constant) a ps) =>
    Default Constant [a] (PGArray p)
  where
    def = let L p = def in rmap PGArray p


------------------------------------------------------------------------------
instance (Columns a p, Default QueryRunner (Column (O.PGArray p)) [a]) =>
    Default (R [] QueryRunner) (Column (O.PGArray p)) a
  where
    def = R def


------------------------------------------------------------------------------
instance (Columns a p, PGArrays p ps, Default (R [] QueryRunner) ps a) =>
    Default QueryRunner (PGArray p) [a]
  where
    def = let R p = def in lmap (\(PGArray a) -> a) p


------------------------------------------------------------------------------
instance IsSqlType a => Default (L [] (->)) (Column a) (Column (O.PGArray a))
  where
    def = L (pgArray id)


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
