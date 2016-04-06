{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Anonymous.Opaleye
    ( PG
    , PGScalar
    , Table'
    , table
    , required
    , optional
    , MakeTableProperties
    , properties
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-types -----------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record, Tuple)
import           Data.Field (Field (Field))
import           Data.Uncurry (Uncurry (Uncurry))


-- anonymous-types-product-profunctors ---------------------------------------
import           Data.Anonymous.Profunctor
                     ( ProductAdaptor
                     , pRecord
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int16, Int32, Int64)
import           Data.Proxy (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable, maybeToNullable)
import           Opaleye.PGTypes
                     ( PGBool
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
                     , pgBool
                     , pgCiStrictText
                     , pgDay
                     , pgDouble
                     , pgInt4
                     , pgInt8
                     , pgLocalTime
                     , pgStrictByteString
                     , pgStrictText
                     , pgTimeOfDay
                     , pgUTCTime
                     , pgValueJSONB
                     )
import           Opaleye.Table (Table (Table), TableProperties)
import qualified Opaleye.Table as O (optional, required)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (dimap)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- time ----------------------------------------------------------------------
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (LocalTime, TimeOfDay)


-- uuid ----------------------------------------------------------------------
import           Data.UUID (UUID)


------------------------------------------------------------------------------
required
    :: forall s a. KnownSymbol s
    => Uncurry Field '(s, TableProperties (Column a) (Column a))
required = Uncurry (Field (O.required (symbolVal (Proxy :: Proxy s))))


------------------------------------------------------------------------------
optional
    :: forall s a. KnownSymbol s
    => Uncurry Field '(s, TableProperties (Maybe (Column a)) (Column a))
optional = Uncurry (Field (O.optional (symbolVal (Proxy :: Proxy s))))


------------------------------------------------------------------------------
class PG a where
    type PGRep a :: *
    pg :: a -> PGRep a


------------------------------------------------------------------------------
instance PG a => PG (Const a b) where
    type PGRep (Const a b) = Const (PGRep a) b
    pg (Const a) = Const (pg a)


------------------------------------------------------------------------------
instance PG a => PG (Identity a) where
    type PGRep (Identity a) = Identity (PGRep a)
    pg (Identity a) = Identity (pg a)


------------------------------------------------------------------------------
instance PG a => PG (Tagged s a) where
    type PGRep (Tagged s a) = Tagged s (PGRep a)
    pg (Tagged a) = Tagged (pg a)


------------------------------------------------------------------------------
instance PG a => PG (Field s a) where
    type PGRep (Field s a) = Field s (PGRep a)
    pg (Field a) = Field (pg a)


------------------------------------------------------------------------------
instance PG () where
    type PGRep () = ()
    pg () = ()


------------------------------------------------------------------------------
instance (PG a, PG b) => PG (a, b) where
    type PGRep (a, b) = (PGRep a, PGRep b)
    pg (a, b) = (pg a, pg b)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c) => PG (a, b, c) where
    type PGRep (a, b, c) = (PGRep a, PGRep b, PGRep c)
    pg (a, b, c) = (pg a, pg b, pg c)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d) => PG (a, b, c, d) where
    type PGRep (a, b, c, d) = (PGRep a, PGRep b, PGRep c, PGRep d)
    pg (a, b, c, d) = (pg a, pg b, pg c, pg d)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e) => PG (a, b, c, d, e) where
    type PGRep (a, b, c, d, e) = (PGRep a, PGRep b, PGRep c, PGRep d, PGRep e)
    pg (a, b, c, d, e) = (pg a, pg b, pg c, pg d, pg e)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f) => PG (a, b, c, d, e, f) where
    type PGRep (a, b, c, d, e, f) =
        (PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f)
    pg (a, b, c, d, e, f) = (pg a, pg b, pg c, pg d, pg e, pg f)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f, PG g) =>
    PG (a, b, c, d, e, f, g)
  where
    type PGRep (a, b, c, d, e, f, g) =
        (PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g)
    pg (a, b, c, d, e, f, g) = (pg a, pg b, pg c, pg d, pg e, pg f, pg g)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h) =>
    PG (a, b, c, d, e, f, g, h)
  where
    type PGRep (a, b, c, d, e, f, g, h) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h
        )
    pg (a, b, c, d, e, f, g, h) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i) =>
    PG (a, b, c, d, e, f, g, h, i)
  where
    type PGRep (a, b, c, d, e, f, g, h, i) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i
        )
    pg (a, b, c, d, e, f, g, h, i) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j) =>
    PG (a, b, c, d, e, f, g, h, i, j)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j
        )
    pg (a, b, c, d, e, f, g, h, i, j) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j)


------------------------------------------------------------------------------
instance (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k) =>
    PG (a, b, c, d, e, f, g, h, i, j, k)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k
        )
    pg (a, b, c, d, e, f, g, h, i, j, k) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k)


------------------------------------------------------------------------------
instance
    (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l)
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o, PG p
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o, PGRep p
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o, pg p
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o, PG p, PG q
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o, PGRep p, PGRep q
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o, pg p, pg q
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o, PG p, PG q, PG r
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o, PGRep p, PGRep q, PGRep r
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o, pg p, pg q, pg r
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o, PG p, PG q, PG r, PG s
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o, PGRep p, PGRep q, PGRep r, PGRep s
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o, pg p, pg q, pg r, pg s
        )


------------------------------------------------------------------------------
instance
    ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k, PG l
    , PG m, PG n, PG o, PG p, PG q, PG r, PG s, PG t
    )
  =>
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  where
    type PGRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
        ( PGRep a, PGRep b, PGRep c, PGRep d, PGRep e, PGRep f, PGRep g
        , PGRep h, PGRep i, PGRep j, PGRep k, PGRep l, PGRep m, PGRep n
        , PGRep o, PGRep p, PGRep q, PGRep r, PGRep s, PGRep t
        )
    pg (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
        ( pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j, pg k
        , pg l, pg m, pg n, pg o, pg p, pg q, pg r, pg s, pg t
        )


------------------------------------------------------------------------------
instance PG (Product g '[]) where
    type PGRep (Product g '[]) = Product g '[]
    pg Nil = Nil


------------------------------------------------------------------------------
instance (PG a, PG (Tuple as), PGRep (Tuple as) ~ Tuple (PGMap as)) =>
    PG (Tuple (a ': as))
  where
    type PGRep (Tuple (a ': as)) = Tuple ((PGRep a ': PGMap as))
    pg (Cons (Identity a) as) = Cons (Identity (pg a)) (pg as)


------------------------------------------------------------------------------
instance (PG a, PG (Record as), PGRep (Record as) ~ Record (PGMapSnd as)) =>
    PG (Record ('(s, a) ': as))
  where
    type PGRep (Record ('(s, a) ': as)) =
        Record ('(s, PGRep a) ': PGMapSnd as)
    pg (Cons (Uncurry (Field a)) as) = Cons (Uncurry (Field (pg a))) (pg as)


------------------------------------------------------------------------------
instance (PG a, PGRep a ~ Column (PGScalar a)) => PG (Maybe a) where
    type PGRep (Maybe a) = Column (Nullable (PGScalar a))
    pg = maybeToNullable . fmap pg


------------------------------------------------------------------------------
instance PG Bool where
    type PGRep Bool = Column PGBool
    pg = pgBool


------------------------------------------------------------------------------
instance PG ByteString where
    type PGRep ByteString = Column PGBytea
    pg = pgStrictByteString


------------------------------------------------------------------------------
instance PG (CI Text) where
    type PGRep (CI Text) = Column PGCitext
    pg = pgCiStrictText


------------------------------------------------------------------------------
instance PG Day where
    type PGRep Day = Column PGDate
    pg = pgDay


------------------------------------------------------------------------------
instance PG Double where
    type PGRep Double = Column PGFloat8
    pg = pgDouble


------------------------------------------------------------------------------
instance PG Int32 where
    type PGRep Int32 = Column PGInt4
    pg = pgInt4 . fromIntegral


------------------------------------------------------------------------------
instance PG Int64 where
    type PGRep Int64 = Column PGInt8
    pg = pgInt8


------------------------------------------------------------------------------
instance PG LocalTime where
    type PGRep LocalTime = Column PGTimestamp
    pg = pgLocalTime


------------------------------------------------------------------------------
instance PG Text where
    type PGRep Text = Column PGText
    pg = pgStrictText


------------------------------------------------------------------------------
instance PG TimeOfDay where
    type PGRep TimeOfDay = Column PGTime
    pg = pgTimeOfDay


------------------------------------------------------------------------------
instance PG UTCTime where
    type PGRep UTCTime = Column PGTimestamptz
    pg = pgUTCTime


------------------------------------------------------------------------------
instance PG Value where
    type PGRep Value = Column PGJsonb
    pg = pgValueJSONB


------------------------------------------------------------------------------
type family PGScalar (a :: *) :: *
type instance PGScalar Bool = PGBool
type instance PGScalar ByteString = PGBytea
type instance PGScalar (CI Text) = PGCitext
type instance PGScalar Day = PGDate
type instance PGScalar Double = PGFloat8
type instance PGScalar Float = PGFloat4
type instance PGScalar Int16 = PGInt2
type instance PGScalar Int32 = PGInt4
type instance PGScalar Int64 = PGInt8
type instance PGScalar LocalTime = PGTimestamp
type instance PGScalar Text = PGText
type instance PGScalar TimeOfDay = PGTime
type instance PGScalar UTCTime = PGTimestamptz
type instance PGScalar UUID = PGUuid
type instance PGScalar Value = PGJsonb
type instance PGScalar (Maybe a) = Nullable (PGScalar a)


{-
------------------------------------------------------------------------------
type family PG (a :: *) :: * where
    PG (Const a b) = Const (PG a) b
    PG (Identity a) = Identity (PG a)
    PG (Tagged s a) = Tagged s (PG a)
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
    PG (a, b, c, d, e, f, g, h, i, j, k) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k)
    PG (a, b, c, d, e, f, g, h, i, j, k, l) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q, PG r
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q, PG r, PG s
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q, PG r, PG s, PG t
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q, PG r, PG s, PG t, PG u
        )
    PG (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =
        ( PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j, PG k
        , PG l, PG m, PG n, PG o, PG p, PG q, PG r, PG s, PG t, PG u, PG v
        )
    PG (Tuple as) = Tuple (PGMap as)
    PG (Record as) = Record (PGMapSnd as)
    PG a = Column (PGScalar a)
-}


------------------------------------------------------------------------------
type family PGMap (as :: [*]) :: [*] where
    PGMap '[] = '[]
    PGMap (a ': as) = PGRep a ': PGMap as


------------------------------------------------------------------------------
type family PGMapSnd (as :: [(s, *)]) :: [(s, *)] where
    PGMapSnd '[] = '[]
    PGMapSnd ('(s, a) ': as) = '(s, PGRep a) ': PGMapSnd as


------------------------------------------------------------------------------
class MakeTableProperties
    (abs :: [(Symbol, *)])
    (as :: [(Symbol, *)])
    (bs :: [(Symbol, *)])
        | abs -> as
        , abs -> bs
  where
    properties :: Record abs


------------------------------------------------------------------------------
instance MakeTableProperties '[] '[] '[] where
    properties = Nil


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties (Column a) (Column a)) ': abs)
        ('(s, Column a) ': as)
        ('(s, Column a) ': bs)
  where
    properties = Cons (required @s @a) properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties (Const (Column a) b) (Const (Column a) b))
            ': abs)
        ('(s, Const (Column a) b) ': as)
        ('(s, Const (Column a) b) ': bs)
  where
    properties = do
        let field = O.required (symbolVal (Proxy :: Proxy s))
        Cons (Uncurry (Field (dimap (\(Const a) -> a) Const field)))
            properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties (Identity (Column a)) (Identity (Column a)))
            ': abs)
        ('(s, Identity (Column a)) ': as)
        ('(s, Identity (Column a)) ': bs)
  where
    properties = do
        let field = O.required (symbolVal (Proxy :: Proxy s))
        Cons (Uncurry (Field (dimap (\(Identity a) -> a) Identity field)))
            properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties (Tagged b (Column a)) (Tagged b (Column a)))
            ': abs)
        ('(s, Tagged b (Column a)) ': as)
        ('(s, Tagged b (Column a)) ': bs)
  where
    properties = do
        let field = O.required (symbolVal (Proxy :: Proxy s))
        Cons (Uncurry (Field (dimap (\(Tagged a) -> a) Tagged field)))
            properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties (Maybe (Column a)) (Column a)) ': abs)
        ('(s, Maybe (Column a)) ': as)
        ('(s, Column a) ': bs)
  where
    properties = Cons (optional @s @a) properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties
            (Maybe (Const (Column a) b))
            (Const (Column a) b))
                ': abs)
        ('(s, Maybe (Const (Column a) b)) ': as)
        ('(s, Const (Column a) b) ': bs)
  where
    properties = do
        let field = O.optional (symbolVal (Proxy :: Proxy s))
        Cons (Uncurry (Field (dimap (fmap (\(Const a) -> a)) Const field)))
            properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties
            (Maybe (Identity (Column a)))
            (Identity (Column a)))
                ': abs)
        ('(s, Maybe (Identity (Column a))) ': as)
        ('(s, Identity (Column a)) ': bs)
  where
    properties = do
        let field = O.optional (symbolVal (Proxy :: Proxy s))
        Cons
            (Uncurry (Field
                (dimap (fmap (\(Identity a) -> a)) Identity field)))
            properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs) =>
    MakeTableProperties
        ('(s, TableProperties
            (Maybe (Tagged b (Column a)))
            (Tagged b (Column a)))
                ': abs)
        ('(s, Maybe (Tagged b (Column a))) ': as)
        ('(s, Tagged b (Column a)) ': bs)
  where
    properties = do
        let field = O.optional (symbolVal (Proxy :: Proxy s))
        Cons (Uncurry (Field (dimap (fmap (\(Tagged a) -> a)) Tagged field)))
            properties


------------------------------------------------------------------------------
type Table' a = Table a a


------------------------------------------------------------------------------
table
    ::
        ( MakeTableProperties abs as bs
        , ProductAdaptor TableProperties (Uncurry Field) abs as bs
        )
    => String
    -> Table (Record as) (Record bs)
table s = Table s $ pRecord properties
