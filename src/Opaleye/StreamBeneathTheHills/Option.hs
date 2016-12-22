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
import           Prelude hiding (null)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable, null, toNullable)
import           Opaleye.PGTypes (IsSqlType, PGArray, pgArray)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap)


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
instance (Profunctor p, Default p (Column (Nullable a)) (Maybe b)) =>
    Default p (Maybe (Column a)) (Maybe b)
  where
    def = lmap (maybe null toNullable) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Const a s) (Maybe (Const b s))
  where
    def = dimap (\(Const a) -> a) (fmap Const) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Identity a) (Maybe (Identity b))
  where
    def = dimap (\(Identity a) -> a) (fmap Identity) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a (Maybe b)) =>
    Default p (Tagged s a) (Maybe (Tagged s b))
  where
    def = dimap (\(Tagged a) -> a) (fmap Tagged) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a (Maybe b), KnownSymbol s) =>
    Default p (Field '(s, a)) (Maybe (Field '(s, b)))
  where
    def = dimap unlabel (fmap (Labeled . Identity)) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Const a s)) (Const b s)
  where
    def = dimap (fmap (\(Const a) -> a)) Const def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Identity a)) (Identity b)
  where
    def = dimap (fmap (\(Identity a) -> a)) Identity def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) b) =>
    Default p (Maybe (Tagged s a)) (Tagged s b)
  where
    def = dimap (fmap (\(Tagged a) -> a)) Tagged def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Maybe a) b, KnownSymbol s) =>
    Default p (Maybe (Field '(s, a))) (Field '(s, b))
  where
    def = dimap (fmap unlabel) (Labeled . Identity) def


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


------------------------------------------------------------------------------
instance (Profunctor p, Default p (Column (PGArray a)) [b], IsSqlType a) =>
    Default p [Column a] [b]
  where
    def = lmap (pgArray id) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a [b]) => Default p (Const a s) [Const b s]
  where
    def = dimap (\(Const a) -> a) (fmap Const) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a [b]) =>
    Default p (Identity a) [Identity b]
  where
    def = dimap (\(Identity a) -> a) (fmap Identity) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a [b]) =>
    Default p (Tagged s a) [Tagged s b]
  where
    def = dimap (\(Tagged a) -> a) (fmap Tagged) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p a [b], KnownSymbol s) =>
    Default p (Field '(s, a)) [Field '(s, b)]
  where
    def = dimap unlabel (fmap (Labeled . Identity)) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] b) => Default p [Const a s] (Const b s)
  where
    def = dimap (fmap (\(Const a) -> a)) Const def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] b) =>
    Default p [Identity a] (Identity b)
  where
    def = dimap (fmap (\(Identity a) -> a)) Identity def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] b) =>
    Default p [Tagged s a] (Tagged s b)
  where
    def = dimap (fmap (\(Tagged a) -> a)) Tagged def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] b, KnownSymbol s) =>
    Default p [Field '(s, a)] (Field '(s, b))
  where
    def = dimap (fmap unlabel) (Labeled . Identity) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] [b]) =>
    Default p [Const a s] [Const b s]
  where
    def = dimap (fmap (\(Const a) -> a)) (fmap Const) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] [b]) =>
    Default p [Identity a] [Identity b]
  where
    def = dimap (fmap (\(Identity a) -> a)) (fmap Identity) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] [b]) =>
    Default p [Tagged s a] [Tagged s b]
  where
    def = dimap (fmap (\(Tagged a) -> a)) (fmap Tagged) def


------------------------------------------------------------------------------
instance (Profunctor p, Default p [a] [b], KnownSymbol s) =>
    Default p [Field '(s, a)] [Field '(s, b)]
  where
    def = dimap (fmap unlabel) (fmap (Labeled . Identity)) def


------------------------------------------------------------------------------
unlabel :: Field '(s, a) -> a
unlabel (Labeled (Identity a)) = a
