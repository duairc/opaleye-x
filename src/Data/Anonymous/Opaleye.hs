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
import           Opaleye.Column (Column, Nullable)
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


------------------------------------------------------------------------------
type family PGMap (as :: [*]) :: [*] where
    PGMap '[] = '[]
    PGMap (a ': as) = PG a ': PGMap as


------------------------------------------------------------------------------
type family PGMapSnd (as :: [(s, *)]) :: [(s, *)] where
    PGMapSnd '[] = '[]
    PGMapSnd ('(s, a) ': as) = '(s, PG a) ': PGMapSnd as


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
type instance PGScalar [a] = PGArray (PGScalar a)
type instance PGScalar (Maybe a) = Nullable (PGScalar a)


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
