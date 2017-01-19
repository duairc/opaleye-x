{-# LANGUAGE CPP #-}
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

module Opaleye.X.Maybe
    ( PGMaybe
    , PGMatchMaybe, pgMatchMaybe
    , PGNothing, pgNothing
    , PGJust, pgJust
    , PGIsJust, pgIsJust, pgIsNothing
    , PGFromJust, pgFromJust
    )
where

-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mempty, mappend)
#endif
import           Data.Semigroup (Semigroup, (<>))
import           GHC.Generics (Generic)
import           Prelude hiding (not, null)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column
                     ( Column
                     , Nullable
                     , isNull
                     , null
                     , toNullable
                     , unsafeCoerceColumn
                     )
import           Opaleye.Constant (Constant, constant)
import           Opaleye.Internal.Operators (IfPP)
import           Opaleye.PGTypes (PGBool)
import           Opaleye.Operators ((.&&), ifThenElseMany, not)
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Internal


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
--newtype PGMaybe a = PGMaybe (DistributeNullable a)
deriving instance Generic (PGMaybe a)
deriving instance Typeable PGMaybe


------------------------------------------------------------------------------
instance (Nullables a as, Nullables b bs, Default p as bs, Profunctor p) =>
    Default p (PGMaybe a) (PGMaybe b)
  where
    def = dimap (\(PGMaybe a) -> a) PGMaybe def


------------------------------------------------------------------------------
instance (Default Constant (Maybe a) (Column (Nullable p))) =>
    Default (L Maybe Constant) a (Column (Nullable p))
  where
    def = L def


------------------------------------------------------------------------------
instance (Nullables p ps, Default (L Maybe Constant) a ps) =>
    Default Constant (Maybe a) (PGMaybe p)
  where
    def = let L p = def in rmap PGMaybe p


------------------------------------------------------------------------------
instance (Default QueryRunner (Column (Nullable p)) (Maybe a)) =>
    Default (R Maybe QueryRunner) (Column (Nullable p)) a
  where
    def = R def


------------------------------------------------------------------------------
instance (Nullables p ps, Default (R Maybe QueryRunner) ps a) =>
    Default QueryRunner (PGMaybe p) (Maybe a)
  where
    def = let R p = def in lmap (\(PGMaybe a) -> a) p


------------------------------------------------------------------------------
instance (PGJust a, PGMatchMaybe a (PGMaybe a)) => Semigroup (PGMaybe a) where
    a <> b = pgMatchMaybe b pgJust a


------------------------------------------------------------------------------
instance (PGNothing a, PGJust a, PGMatchMaybe a (PGMaybe a)) =>
    Monoid (PGMaybe a)
  where
    mempty = pgNothing
    mappend a b = pgMatchMaybe b pgJust a


------------------------------------------------------------------------------
newtype NothingPP a b = NothingPP b


------------------------------------------------------------------------------
instance Profunctor NothingPP where
    dimap _ r (NothingPP p) = NothingPP (r p)


------------------------------------------------------------------------------
instance ProductProfunctor NothingPP where
    empty = NothingPP ()
    NothingPP a ***! NothingPP b = NothingPP (a, b)


------------------------------------------------------------------------------
instance Default NothingPP (Column (Nullable a)) (Column (Nullable a)) where
    def = NothingPP null


------------------------------------------------------------------------------
type PGNothing a =
    ( Nullables a (DistributeNullable a)
    , Default NothingPP (DistributeNullable a) (DistributeNullable a)
    )


------------------------------------------------------------------------------
pgNothing :: forall a. PGNothing a => PGMaybe a
pgNothing = let NothingPP n = p in PGMaybe n
  where
    p = def :: NothingPP (DistributeNullable a) (DistributeNullable a)


------------------------------------------------------------------------------
newtype JustPP a b = JustPP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default JustPP (Column a) (Column (Nullable a)) where
    def = JustPP toNullable


------------------------------------------------------------------------------
type PGJust a =
    ( Nullables a (DistributeNullable a)
    , Default JustPP a (DistributeNullable a)
    )


------------------------------------------------------------------------------
pgJust :: PGJust a => a -> PGMaybe a
pgJust = let JustPP f = def in PGMaybe . f


------------------------------------------------------------------------------
newtype FromJustPP a b = FromJustPP (a -> b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default FromJustPP (Column (Nullable a)) (Column a) where
    def = FromJustPP unsafeCoerceColumn


------------------------------------------------------------------------------
type PGFromJust a =
    ( Nullables a (DistributeNullable a)
    , Default FromJustPP (DistributeNullable a) a
    )


------------------------------------------------------------------------------
pgFromJust :: PGFromJust a => PGMaybe a -> a
pgFromJust = let FromJustPP f = def in f . (\(PGMaybe a) -> a)


------------------------------------------------------------------------------
newtype IsJustPP a b = IsJustPP (a -> Column PGBool)


------------------------------------------------------------------------------
instance Profunctor IsJustPP where
    dimap l _ (IsJustPP p) = IsJustPP (lmap l p)


------------------------------------------------------------------------------
instance ProductProfunctor IsJustPP where
    empty = IsJustPP (const (constant True))
    IsJustPP a ***! IsJustPP b = IsJustPP (rmap (uncurry (.&&)) (a ***! b))


------------------------------------------------------------------------------
instance Default IsJustPP (Column (Nullable a)) (Column (Nullable a)) where
    def = IsJustPP (not . isNull)


------------------------------------------------------------------------------
type PGIsJust a =
    ( Nullables a (DistributeNullable a)
    , Default IsJustPP (DistributeNullable a) (DistributeNullable a)
    )


------------------------------------------------------------------------------
pgIsJust :: forall a. PGIsJust a => PGMaybe a -> Column PGBool
pgIsJust (PGMaybe a) = let IsJustPP f = p in f a
  where
    p = def :: IsJustPP (DistributeNullable a) (DistributeNullable a)


------------------------------------------------------------------------------
pgIsNothing :: PGIsJust a => PGMaybe a -> Column PGBool
pgIsNothing = not . pgIsJust


------------------------------------------------------------------------------
type PGMatchMaybe a b =
    ( Nullables a (DistributeNullable a)
    , PGIsJust a
    , Default FromJustPP (DistributeNullable a) a
    , Default IfPP b b
    )


------------------------------------------------------------------------------
pgMatchMaybe :: PGMatchMaybe a b => b -> (a -> b) -> PGMaybe a -> b
pgMatchMaybe b f a = ifThenElseMany (pgIsNothing a) b (f (pgFromJust a))
