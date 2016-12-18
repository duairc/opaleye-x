{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.StreamBeneathTheHills.Option (Option (Option)) where

-- anonymous-data ------------------------------------------------------------
import           Data.Labeled (Labeled (Labeled), Field)


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const), Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Monad (MonadPlus)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Foldable (Foldable)
#endif
import           Data.Functor.Identity (Identity (Identity))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (Traversable)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup)
#endif
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           GHC.TypeLits (KnownSymbol)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default, def)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


------------------------------------------------------------------------------
newtype Option a = Option (Maybe a) deriving
    ( Functor, Foldable, Traversable, Applicative, Monad, Alternative
    , MonadPlus, Eq, Ord, Read, Show, Typeable, Generic, Generic1, Monoid
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup
#endif
    )


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Option a) (Option b)
  where
    def = dimap (\(Option a) -> a) Option def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Const a s)) (Maybe (Const b s))
  where
    def = dimap (fmap (\(Const a) -> a)) (fmap Const) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Identity a)) (Maybe (Identity b))
  where
    def = dimap (fmap (\(Identity a) -> a)) (fmap Identity) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) (Maybe b)) =>
    Default p (Maybe (Tagged s a)) (Maybe (Tagged s b))
  where
    def = dimap (fmap (\(Tagged a) -> a)) (fmap Tagged) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) (Maybe b), KnownSymbol s) =>
    Default p (Maybe (Field '(s, a))) (Maybe (Field '(s, b)))
  where
    def = dimap (fmap unlabel) (fmap (Labeled . Identity)) def
      where
        unlabel :: Field '(s, a) -> a
        unlabel (Labeled (Identity a)) = a
