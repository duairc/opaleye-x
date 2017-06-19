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

module Opaleye.X.Cast
    ( Cast, cast
    )
where

-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, unsafeCoerceColumn)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
newtype CastPP a b = CastPP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default CastPP (Column a) (Column b) where
    def = CastPP unsafeCoerceColumn


------------------------------------------------------------------------------
type Cast = Default CastPP


------------------------------------------------------------------------------
cast :: Cast a b => a -> b
cast = let CastPP p = def in p
