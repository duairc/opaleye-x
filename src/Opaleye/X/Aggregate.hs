{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.X.Aggregate
    ( ArrayAgg, arrayAgg
    , GroupBy, groupBy
    )
where

-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.Aggregate (Aggregator)
import qualified Opaleye.Aggregate as O (groupBy)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Array (ArrayAgg, arrayAgg)
import           Opaleye.X.TF ()


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
newtype GroupByPP a b = GroupByPP (Aggregator a b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default GroupByPP (Column a) (Column a) where
    def = GroupByPP O.groupBy


------------------------------------------------------------------------------
type GroupBy = Default GroupByPP


------------------------------------------------------------------------------
groupBy :: GroupBy a b => Aggregator a b
groupBy = let GroupByPP p = def in p
