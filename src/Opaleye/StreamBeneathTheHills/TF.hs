{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 710
#define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
#else
#define __OVERLAPPABLE__
{-# LANGUAGE OverlappingInstances #-}
#endif

module Opaleye.StreamBeneathTheHills.TF
    ( PGScalar
    , UnPGScalar
    , PG
    , UnPG

    , PGRep
    , ToPG
    , FromPG
    , pg

    , PGMap
    , UnPGMap
    , PGMapSnd
    , UnPGMapSnd
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product, Record, Tuple)
import           Data.Labeled (Field)


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32, Int64)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable)
import           Opaleye.Constant (Constant, constant)
import           Opaleye.PGTypes
                     ( PGArray
                     , PGBool
                     , PGBytea
                     , PGCitext
                     , PGDate
                     , PGFloat4
                     , PGFloat8
                     , PGInt2
                     , PGInt4
                     , PGInt8
                     , PGJsonb
                     , PGText
                     , PGTime
                     , PGTimestamp
                     , PGTimestamptz
                     , PGUuid
                     )
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Option (Option)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- time ----------------------------------------------------------------------
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (LocalTime, TimeOfDay)


-- uuid-types ----------------------------------------------------------------
import           Data.UUID.Types (UUID)


------------------------------------------------------------------------------
type family PGScalar (a :: *) :: * where
    PGScalar Bool = PGBool
    PGScalar ByteString = PGBytea
    PGScalar (CI Text) = PGCitext
    PGScalar Day = PGDate
    PGScalar Double = PGFloat8
    PGScalar Float = PGFloat4
    PGScalar Int16 = PGInt2
    PGScalar Int32 = PGInt4
    PGScalar Int64 = PGInt8
    PGScalar LocalTime = PGTimestamp
    PGScalar Text = PGText
    PGScalar TimeOfDay = PGTime
    PGScalar UTCTime = PGTimestamptz
    PGScalar UUID = PGUuid
    PGScalar Value = PGJsonb


------------------------------------------------------------------------------
type family UnPGScalar (a :: *) :: * where
    UnPGScalar PGBool = Bool
    UnPGScalar PGBytea = ByteString
    UnPGScalar PGCitext = (CI Text)
    UnPGScalar PGDate = Day
    UnPGScalar PGFloat8 = Double
    UnPGScalar PGFloat4 = Float
    UnPGScalar PGInt2 = Int16
    UnPGScalar PGInt4 = Int32
    UnPGScalar PGInt8 = Int64
    UnPGScalar PGTimestamp = LocalTime
    UnPGScalar PGText = Text
    UnPGScalar PGTime = TimeOfDay
    UnPGScalar PGTimestamptz = UTCTime
    UnPGScalar PGUuid = UUID
    UnPGScalar PGJsonb = Value


------------------------------------------------------------------------------
type family PG a :: * where
    PG () = ()
    PG (a, b) = (PG a, PG b)
    PG (a, b, c) = (PG a, PG b, PG c)
    PG (a, b, c, d) = (PG a, PG b, PG c, PG d)
    PG (a, b, c, d, e) = (PG a, PG b, PG c, PG d, PG e)
    PG (a, b, c, d, e, f) = (PG a, PG b, PG c, PG d, PG e, PG f)
    PG (a, b, c, d, e, f, g) = (PG a, PG b, PG c, PG d, PG e, PG f, PG g)
    PG (a, b, c, d, e, f, g, h) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h)
    PG (a, b, c, d, e, f, g, h, i) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i)
    PG (a, b, c, d, e, f, g, h, i, j) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j)
    PG (Product g '[]) = Product g '[]
    PG (Tuple (a ': as)) = Tuple (PG a ': PGMap as)
    PG (Record ('(s, a) ': as)) = Record ('(s, PG a) ': PGMapSnd as)
    PG (Option a) = Option (PG a)
    PG (Const a b) = Const (PG a) b
    PG (Identity a) = Identity (PG a)
    PG (Tagged s a) = Tagged s (PG a)
    PG (Field '(s, a)) = Field '(s, PG a)
    PG (Maybe (Const a b)) = Const (PG (Maybe a)) b
    PG (Maybe (Identity a)) = Identity (PG (Maybe a))
    PG (Maybe (Tagged s a)) = Tagged s (PG (Maybe a))
    PG (Maybe (Field '(s, a))) = Field '(s, PG (Maybe a))
    PG [Const a b] = Const (PG [a]) b
    PG [Identity a] = Identity (PG [a])
    PG [Tagged s a] = Tagged s (PG [a])
    PG [Field '(s, a)] = Field '(s, PG [a])
    PG (Maybe a) = Column (Nullable (PGScalar a))
    PG [a] = Column (PGArray (PGScalar a))
    PG a = Column (PGScalar a)


------------------------------------------------------------------------------
type family UnPG a :: * where
    UnPG () = ()
    UnPG (a, b) = (UnPG a, UnPG b)
    UnPG (a, b, c) = (UnPG a, UnPG b, UnPG c)
    UnPG (a, b, c, d) = (UnPG a, UnPG b, UnPG c, UnPG d)
    UnPG (a, b, c, d, e) = (UnPG a, UnPG b, UnPG c, UnPG d, UnPG e)
    UnPG (a, b, c, d, e, f) = (UnPG a, UnPG b, UnPG c, UnPG d, UnPG e, UnPG f)
    UnPG (a, b, c, d, e, f, g) =
        (UnPG a, UnPG b, UnPG c, UnPG d, UnPG e, UnPG f, UnPG g)
    UnPG (a, b, c, d, e, f, g, h) =
        (UnPG a, UnPG b, UnPG c, UnPG d, UnPG e, UnPG f, UnPG g, UnPG h)
    UnPG (a, b, c, d, e, f, g, h, i) =
        ( UnPG a, UnPG b, UnPG c, UnPG d, UnPG e, UnPG f, UnPG g, UnPG h
        , UnPG i
        )
    UnPG (a, b, c, d, e, f, g, h, i, j) =
        ( UnPG a, UnPG b, UnPG c, UnPG d, UnPG e, UnPG f, UnPG g, UnPG h
        , UnPG i, UnPG j
        )
    UnPG (Product g '[]) = Product g '[]
    UnPG (Tuple (a ': as)) = Tuple (UnPG a ': UnPGMap as)
    UnPG (Record ('(s, a) ': as)) = Record ('(s, UnPG a) ': UnPGMapSnd as)
    UnPG (Option a) = Option (UnPG a)
    UnPG (Const (Column (Nullable a)) b) = Maybe (Const (UnPGScalar a) b)
    UnPG (Identity (Column (Nullable a))) = Maybe (Identity (UnPGScalar a))
    UnPG (Tagged s (Column (Nullable a))) = Maybe (Tagged s (UnPGScalar a))
    UnPG (Field '(s, Column (Nullable a))) = Maybe (Field '(s, UnPGScalar a))
    UnPG (Const (Column (PGArray a)) b) = [Const (UnPGScalar a) b]
    UnPG (Identity (Column (PGArray a))) = [Identity (UnPGScalar a)]
    UnPG (Tagged s (Column (PGArray a))) = [Tagged s (UnPGScalar a)]
    UnPG (Field '(s, Column (PGArray a))) = [Field '(s, UnPGScalar a)]
    UnPG (Const a b) = Const (UnPG a) b
    UnPG (Identity a) = Identity (UnPG a)
    UnPG (Tagged s a) = Tagged s (UnPG a)
    UnPG (Field '(s, a)) = Field '(s, UnPG a)
    UnPG (Column (PGArray p)) = [UnPGScalar p]
    UnPG (Column (Nullable a)) = Maybe (UnPGScalar a)
    UnPG (Column a) = UnPGScalar a


------------------------------------------------------------------------------
type family PGMap (as :: [*]) :: [*] where
    PGMap '[] = '[]
    PGMap (a ': as) = PG a ': PGMap as


------------------------------------------------------------------------------
type family PGMapSnd (as :: [(k, *)]) :: [(k, *)] where
    PGMapSnd '[] = '[]
    PGMapSnd ('(s, a) ': as) = '(s, PG a) ': PGMapSnd as


------------------------------------------------------------------------------
type family UnPGMap (as :: [*]) :: [*] where
    UnPGMap '[] = '[]
    UnPGMap (a ': as) = UnPG a ': UnPGMap as


------------------------------------------------------------------------------
type family UnPGMapSnd (as :: [(k, *)]) :: [(k, *)] where
    UnPGMapSnd '[] = '[]
    UnPGMapSnd ('(s, a) ': as) = '(s, UnPG a) ': UnPGMapSnd as


------------------------------------------------------------------------------
class (PG a ~ p, UnPG p ~ a) => PGRep a p | a -> p , p -> a
instance (PG a ~ p, UnPG p ~ a) => PGRep a p


------------------------------------------------------------------------------
class (Default Constant a p, PGRep a p) => ToPG a p | a -> p, p -> a
instance (Default Constant a p, PGRep a p) => ToPG a p


------------------------------------------------------------------------------
class (Default QueryRunner p a, PGRep a p) => FromPG p a | a -> p, p -> a
instance (Default QueryRunner p a, PGRep a p) => FromPG p a


------------------------------------------------------------------------------
pg :: ToPG a p => a -> p
pg = constant
