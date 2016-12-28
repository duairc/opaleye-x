{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.StreamBeneathTheHills.Optional
    ( Optional
    , MatchOptional, matchOptional
    , Defaults, defaults
    , Override, override
    , Optionalize, optionalize
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Monad (MonadPlus)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Foldable (Foldable)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Traversable (Traversable)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid)
#endif
import           Data.Semigroup (Semigroup)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)


-- opaleye-of-the-stream-beheath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Internal


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
--newtype Option a = Option (Maybe a)
deriving instance Functor Option
deriving instance Foldable Option
deriving instance Traversable Option
deriving instance Applicative Option
deriving instance Alternative Option
deriving instance Monad Option
deriving instance MonadPlus Option
deriving instance Eq a => Eq (Option a)
deriving instance Ord a => Ord (Option a)
deriving instance Read a => Read (Option a)
deriving instance Show a => Show (Option a)
deriving instance Monoid a => Monoid (Option a)
deriving instance Semigroup a => Semigroup (Option a)
deriving instance Generic (Option a)
deriving instance Generic1 Option
deriving instance Typeable Option


------------------------------------------------------------------------------
instance (Default p (Maybe a) (Maybe b), Profunctor p) =>
    Default p (Option a) (Option b)
  where
    def = dimap (\(Option a) -> a) Option def


------------------------------------------------------------------------------
instance Default (R Maybe (->)) (Option a) a where
    def = R (\(Option a) -> a)


------------------------------------------------------------------------------
--newtype Optional a = Optional (DistributeOption a)
deriving instance Generic (Optional a)
deriving instance Typeable Optional


------------------------------------------------------------------------------
instance (Options a oa, Options b ob, Default p oa ob, Profunctor p) =>
    Default p (Optional a) (Optional b)
  where
    def = dimap (\(Optional a) -> a) Optional def


------------------------------------------------------------------------------
type MatchOptional a =
    ( Options a (DistributeOption a)
    , Default (R Maybe (->)) (DistributeOption a) a
    )


------------------------------------------------------------------------------
matchOptional :: MatchOptional a => b -> (a -> b) -> Optional a -> b
matchOptional b f (Optional a) = let R p = def in maybe b f (p a)


------------------------------------------------------------------------------
newtype DefaultsPP a b = DefaultsPP b


------------------------------------------------------------------------------
instance Profunctor DefaultsPP where
    dimap _ r (DefaultsPP p) = DefaultsPP (r p)


------------------------------------------------------------------------------
instance ProductProfunctor DefaultsPP where
    empty = DefaultsPP ()
    DefaultsPP a ***! DefaultsPP b = DefaultsPP (a, b)


------------------------------------------------------------------------------
instance Default DefaultsPP (Option a) (Option a) where
    def = DefaultsPP (Option Nothing)


------------------------------------------------------------------------------
type Defaults a =
    ( Options a (DistributeOption a)
    , Default DefaultsPP (DistributeOption a) (DistributeOption a)
    )


------------------------------------------------------------------------------
defaults :: forall a. Defaults a => Optional a
defaults = let DefaultsPP n = p in Optional n
  where
    p = def :: DefaultsPP (DistributeOption a) (DistributeOption a)


------------------------------------------------------------------------------
newtype OverridePP a b = OverridePP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default OverridePP a (Option a) where
    def = OverridePP (Option . Just)


------------------------------------------------------------------------------
type Override a =
    ( Options a (DistributeOption a)
    , Default OverridePP a (DistributeOption a)
    )


------------------------------------------------------------------------------
override :: Override a => a -> Optional a
override = let OverridePP f = def in Optional . f


------------------------------------------------------------------------------
newtype OptionalizePP a b = OptionalizePP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance __INCOHERENT__ Default OptionalizePP a a where
    def = OptionalizePP id


------------------------------------------------------------------------------
instance Default OptionalizePP a (Option a) where
    def = OptionalizePP (Option . Just)


------------------------------------------------------------------------------
instance __OVERLAPS__
    (Options a oa, Options b ob, Default OptionalizePP oa ob)
  =>
    Default OptionalizePP (Optional a) (Optional b)
  where
    def = dimap (\(Optional a) -> a) Optional def


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Options b o, Default OptionalizePP a o) =>
    Default OptionalizePP a (Optional b)
  where
    def = let OptionalizePP p = def in OptionalizePP $ Optional . p


------------------------------------------------------------------------------
type Optionalize a b = (Optionals a b, Default OptionalizePP a b)


------------------------------------------------------------------------------
optionalize :: Optionalize a b => a -> b
optionalize = let OptionalizePP p = def in p
