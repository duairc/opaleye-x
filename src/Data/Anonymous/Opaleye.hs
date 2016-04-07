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
    ( PGRun
    , PG
    , UnPG
    , run
    , PGLift
    , pg
    , PGScalar
    , UnPGScalar
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
import           Data.Typeable (Typeable)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable, maybeToNullable)
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
import           Opaleye.QueryArr (Query)
import           Opaleye.RunQuery
                     ( QueryRunner
                     , QueryRunnerColumnDefault
                     , runQuery
                     )
import           Opaleye.Table (Table (Table), TableProperties)
import qualified Opaleye.Table as O (optional, required)


-- postgresql-simple ---------------------------------------------------------
import           Database.PostgreSQL.Simple (Connection)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (dimap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


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
class (PG a ~ p, UnPG p ~ a, Default QueryRunner p a) =>
    PGRun a p | p -> a, a -> p
  where
    type PG a :: *
    type UnPG p :: *
    run :: Connection -> Query p -> IO [a]
    run = runQuery


------------------------------------------------------------------------------
class PGRun a p => PGLift a p | p -> a, a -> p where
    pg :: a -> p


------------------------------------------------------------------------------
instance PGRun a p => PGRun (Const a b) (Const p b) where
    type PG (Const a b) = Const (PG a) b
    type UnPG (Const p b) = Const (UnPG p) b


------------------------------------------------------------------------------
instance PGLift a p => PGLift (Const a b) (Const p b) where
    pg (Const a) = Const (pg a)


------------------------------------------------------------------------------
instance PGRun a p => PGRun (Identity a) (Identity p) where
    type PG (Identity a) = Identity (PG a)
    type UnPG (Identity a) = Identity (UnPG a)


------------------------------------------------------------------------------
instance PGLift a p => PGLift (Identity a) (Identity p) where
    pg (Identity a) = Identity (pg a)


------------------------------------------------------------------------------
instance PGRun a p => PGRun (Tagged s a) (Tagged s p) where
    type PG (Tagged s a) = Tagged s (PG a)
    type UnPG (Tagged s p) = Tagged s (UnPG p)


------------------------------------------------------------------------------
instance PGLift a p => PGLift (Tagged s a) (Tagged s p) where
    pg (Tagged a) = Tagged (pg a)


------------------------------------------------------------------------------
instance (KnownSymbol s, PGRun a p) => PGRun (Field s a) (Field s p) where
    type PG (Field s a) = Field s (PG a)
    type UnPG (Field s p) = Field s (UnPG p)


------------------------------------------------------------------------------
instance (KnownSymbol s, PGLift a p) => PGLift (Field s a) (Field s p) where
    pg (Field a) = Field (pg a)


------------------------------------------------------------------------------
instance PGRun () () where
    type PG () = ()
    type UnPG () = ()


------------------------------------------------------------------------------
instance PGLift () () where
    pg () = ()


------------------------------------------------------------------------------
instance (PGRun a pa, PGRun b pb) => PGRun (a, b) (pa, pb) where
    type PG (a, b) = (PG a, PG b)
    type UnPG (pa, pb) = (UnPG pa, UnPG pb)


------------------------------------------------------------------------------
instance (PGLift a pa, PGLift b pb) => PGLift (a, b) (pa, pb) where
    pg (a, b) = (pg a, pg b)


------------------------------------------------------------------------------
instance (PGRun a pa, PGRun b pb, PGRun c pc) => PGRun (a, b, c) (pa, pb, pc)
  where
    type PG (a, b, c) = (PG a, PG b, PG c)
    type UnPG (pa, pb, pc) = (UnPG pa, UnPG pb, UnPG pc)


------------------------------------------------------------------------------
instance (PGLift a pa, PGLift b pb, PGLift c pc) =>
    PGLift (a, b, c) (pa, pb, pc)
  where
    pg (a, b, c) = (pg a, pg b, pg c)


------------------------------------------------------------------------------
instance (PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd) =>
    PGRun (a, b, c, d) (pa, pb, pc, pd)
  where
    type PG (a, b, c, d) = (PG a, PG b, PG c, PG d)
    type UnPG (pa, pb, pc, pd) = (UnPG pa, UnPG pb, UnPG pc, UnPG pd)


------------------------------------------------------------------------------
instance (PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd) =>
    PGLift (a, b, c, d) (pa, pb, pc, pd)
  where
    pg (a, b, c, d) = (pg a, pg b, pg c, pg d)


------------------------------------------------------------------------------
instance (PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe) =>
    PGRun (a, b, c, d, e) (pa, pb, pc, pd, pe)
  where
    type PG (a, b, c, d, e) = (PG a, PG b, PG c, PG d, PG e)
    type UnPG (pa, pb, pc, pd, pe) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe)


------------------------------------------------------------------------------
instance (PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe) =>
    PGLift (a, b, c, d, e) (pa, pb, pc, pd, pe)
  where
    pg (a, b, c, d, e) = (pg a, pg b, pg c, pg d, pg e)


------------------------------------------------------------------------------
instance
    (PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe, PGRun f pf)
  =>
    PGRun (a, b, c, d, e, f) (pa, pb, pc, pd, pe, pf)
  where
    type PG (a, b, c, d, e, f) = (PG a, PG b, PG c, PG d, PG e, PG f)
    type UnPG (pa, pb, pc, pd, pe, pf) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf)


------------------------------------------------------------------------------
instance
    ( PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe
    , PGLift f pf
    )
  =>
    PGLift (a, b, c, d, e, f) (pa, pb, pc, pd, pe, pf)
  where
    pg (a, b, c, d, e, f) = (pg a, pg b, pg c, pg d, pg e, pg f)


------------------------------------------------------------------------------
instance
    ( PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe, PGRun f pf
    , PGRun g pg
    )
  =>
    PGRun (a, b, c, d, e, f, g) (pa, pb, pc, pd, pe, pf, pg)
  where
    type PG (a, b, c, d, e, f, g) = (PG a, PG b, PG c, PG d, PG e, PG f, PG g)
    type UnPG (pa, pb, pc, pd, pe, pf, pg) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg)


------------------------------------------------------------------------------
instance
    ( PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe
    , PGLift f pf, PGLift g pg
    )
  =>
    PGLift (a, b, c, d, e, f, g) (pa, pb, pc, pd, pe, pf, pg)
  where
    pg (a, b, c, d, e, f, g) = (pg a, pg b, pg c, pg d, pg e, pg f, pg g)


------------------------------------------------------------------------------
instance
    ( PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe, PGRun f pf
    , PGRun g pg, PGRun h ph
    )
  =>
    PGRun (a, b, c, d, e, f, g, h) (pa, pb, pc, pd, pe, pf, pg, ph)
  where
    type PG (a, b, c, d, e, f, g, h) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h)
    type UnPG (pa, pb, pc, pd, pe, pf, pg, ph) =
        ( UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg
        , UnPG ph
        )


------------------------------------------------------------------------------
instance
    ( PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe
    , PGLift f pf, PGLift g pg, PGLift h ph
    )
  =>
    PGLift (a, b, c, d, e, f, g, h) (pa, pb, pc, pd, pe, pf, pg, ph)
  where
    pg (a, b, c, d, e, f, g, h) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h)


------------------------------------------------------------------------------
instance
    ( PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe, PGRun f pf
    , PGRun g pg, PGRun h ph, PGRun i pi
    )
  =>
    PGRun (a, b, c, d, e, f, g, h, i) (pa, pb, pc, pd, pe, pf, pg, ph, pi)
  where
    type PG (a, b, c, d, e, f, g, h, i) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i)
    type UnPG (pa, pb, pc, pd, pe, pf, pg, ph, pi) =
        ( UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg
        , UnPG ph, UnPG pi
        )


------------------------------------------------------------------------------
instance
    ( PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe
    , PGLift f pf, PGLift g pg, PGLift h ph, PGLift i pi
    )
  =>
    PGLift (a, b, c, d, e, f, g, h, i) (pa, pb, pc, pd, pe, pf, pg, ph, pi)
  where
    pg (a, b, c, d, e, f, g, h, i) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i)


------------------------------------------------------------------------------
instance
    ( PGRun a pa, PGRun b pb, PGRun c pc, PGRun d pd, PGRun e pe, PGRun f pf
    , PGRun g pg, PGRun h ph, PGRun i pi, PGRun j pj
    )
  =>
    PGRun
        (a, b, c, d, e, f, g, h, i, j)
        (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj)
  where
    type PG (a, b, c, d, e, f, g, h, i, j) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i, PG j)
    type UnPG (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj) =
        ( UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg
        , UnPG ph, UnPG pi, UnPG pj
        )


------------------------------------------------------------------------------
instance
    ( PGLift a pa, PGLift b pb, PGLift c pc, PGLift d pd, PGLift e pe
    , PGLift f pf, PGLift g pg, PGLift h ph, PGLift i pi, PGLift j pj
    )
  =>
    PGLift
        (a, b, c, d, e, f, g, h, i, j)
        (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj)
  where
    pg (a, b, c, d, e, f, g, h, i, j) =
        (pg a, pg b, pg c, pg d, pg e, pg f, pg g, pg h, pg i, pg j)


------------------------------------------------------------------------------
instance PGRun (Product g '[]) (Product g '[]) where
    type PG (Product g '[]) = Product g '[]
    type UnPG (Product g '[]) = Product g '[]


------------------------------------------------------------------------------
instance PGLift (Product g '[]) (Product g '[]) where
    pg Nil = Nil


------------------------------------------------------------------------------
instance
    ( PGRun a p
    , PGRun (Tuple as) (Tuple ps)
    , PG (Tuple as) ~ Tuple (PGMap as)
    , UnPG (Tuple ps) ~ Tuple (UnPGMap ps)
    )
  =>
    PGRun (Tuple (a ': as)) (Tuple (p ': ps))
  where
    type PG (Tuple (a ': as)) = Tuple (PG a ': PGMap as)
    type UnPG (Tuple (p ': ps)) = Tuple (UnPG p ': UnPGMap ps)


------------------------------------------------------------------------------
instance
    ( PGLift a p
    , PGLift (Tuple as) (Tuple ps)
    , PG (Tuple as) ~ Tuple (PGMap as)
    , UnPG (Tuple ps) ~ Tuple (UnPGMap ps)
    )
  =>
    PGLift (Tuple (a ': as)) (Tuple (p ': ps))
  where
    pg (Cons (Identity a) as) = Cons (Identity (pg a)) (pg as)


------------------------------------------------------------------------------
instance
    ( PGRun a p
    , PGRun (Record as) (Record ps)
    , PG (Record as) ~ Record (PGMapSnd as)
    , UnPG (Record ps) ~ Record (UnPGMapSnd ps)
    , KnownSymbol s
    )
  =>
    PGRun (Record ('(s, a) ': as)) (Record ('(s, p) ': ps))
  where
    type PG (Record ('(s, a) ': as)) = Record ('(s, PG a) ': PGMapSnd as)
    type UnPG (Record ('(s, p) ': ps)) =
        Record ('(s, UnPG p) ': UnPGMapSnd ps)


------------------------------------------------------------------------------
instance
    ( PGLift a p
    , PGLift (Record as) (Record ps)
    , PG (Record as) ~ Record (PGMapSnd as)
    , UnPG (Record ps) ~ Record (UnPGMapSnd ps)
    , KnownSymbol s
    )
  =>
    PGLift (Record ('(s, a) ': as)) (Record ('(s, p) ': ps))
  where
    pg (Cons (Uncurry (Field a)) as) = Cons (Uncurry (Field (pg a))) (pg as)


------------------------------------------------------------------------------
instance
    ( PGRun a (Column p)
    , p ~ PGScalar a
    , a ~ UnPGScalar p
    , QueryRunnerColumnDefault p a
    )
  =>
    PGRun (Maybe a) (Column (Nullable p))
  where
    type PG (Maybe a) = Column (Nullable (PGScalar a))
    type UnPG (Column (Nullable p)) = Maybe (UnPGScalar p)


------------------------------------------------------------------------------
instance
    ( PGLift a (Column p)
    , p ~ PGScalar a
    , a ~ UnPGScalar p
    , QueryRunnerColumnDefault p a
    )
  =>
    PGLift (Maybe a) (Column (Nullable p))
  where
    pg = maybeToNullable . fmap pg


------------------------------------------------------------------------------
instance
    ( PGRun a (Column p)
    , p ~ PGScalar a
    , a ~ UnPGScalar p
    , QueryRunnerColumnDefault p a
    , Typeable a
    )
  =>
    PGRun [a] (Column (PGArray p))
  where
    type PG [a] = Column (PGArray (PGScalar a))
    type UnPG (Column (PGArray p)) = [UnPGScalar p]


------------------------------------------------------------------------------
instance PGRun Bool (Column PGBool) where
    type PG Bool = Column PGBool
    type UnPG (Column PGBool) = Bool


------------------------------------------------------------------------------
instance PGLift Bool (Column PGBool) where
    pg = pgBool


------------------------------------------------------------------------------
instance PGRun ByteString (Column PGBytea) where
    type PG ByteString = Column PGBytea
    type UnPG (Column PGBytea) = ByteString


------------------------------------------------------------------------------
instance PGLift ByteString (Column PGBytea) where
    pg = pgStrictByteString


------------------------------------------------------------------------------
instance PGRun (CI Text) (Column PGCitext) where
    type PG (CI Text) = Column PGCitext
    type UnPG (Column PGCitext) = CI Text


------------------------------------------------------------------------------
instance PGLift (CI Text) (Column PGCitext) where
    pg = pgCiStrictText


------------------------------------------------------------------------------
instance PGRun Day (Column PGDate) where
    type PG Day = Column PGDate
    type UnPG (Column PGDate) = Day


------------------------------------------------------------------------------
instance PGLift Day (Column PGDate) where
    pg = pgDay


------------------------------------------------------------------------------
instance PGRun Double (Column PGFloat8) where
    type PG Double = Column PGFloat8
    type UnPG (Column PGFloat8) = Double


------------------------------------------------------------------------------
instance PGLift Double (Column PGFloat8) where
    pg = pgDouble


------------------------------------------------------------------------------
instance PGRun Int32 (Column PGInt4) where
    type PG Int32 = Column PGInt4
    type UnPG (Column PGInt4) = Int32


------------------------------------------------------------------------------
instance PGLift Int32 (Column PGInt4) where
    pg = pgInt4 . fromIntegral


------------------------------------------------------------------------------
instance PGRun Int64 (Column PGInt8) where
    type PG Int64 = Column PGInt8
    type UnPG (Column PGInt8) = Int64


------------------------------------------------------------------------------
instance PGLift Int64 (Column PGInt8) where
    pg = pgInt8


------------------------------------------------------------------------------
instance PGRun LocalTime (Column PGTimestamp) where
    type PG LocalTime = Column PGTimestamp
    type UnPG (Column PGTimestamp) = LocalTime


------------------------------------------------------------------------------
instance PGLift LocalTime (Column PGTimestamp) where
    pg = pgLocalTime


------------------------------------------------------------------------------
instance PGRun Text (Column PGText) where
    type PG Text = Column PGText
    type UnPG (Column PGText) = Text


------------------------------------------------------------------------------
instance PGLift Text (Column PGText) where
    pg = pgStrictText


------------------------------------------------------------------------------
instance PGRun TimeOfDay (Column PGTime) where
    type PG TimeOfDay = Column PGTime
    type UnPG (Column PGTime) = TimeOfDay


------------------------------------------------------------------------------
instance PGLift TimeOfDay (Column PGTime) where
    pg = pgTimeOfDay


------------------------------------------------------------------------------
instance PGRun UTCTime (Column PGTimestamptz) where
    type PG UTCTime = Column PGTimestamptz
    type UnPG (Column PGTimestamptz) = UTCTime


------------------------------------------------------------------------------
instance PGLift UTCTime (Column PGTimestamptz) where
    pg = pgUTCTime


------------------------------------------------------------------------------
instance PGRun Value (Column PGJsonb) where
    type PG Value = Column PGJsonb
    type UnPG (Column PGJsonb) = Value


------------------------------------------------------------------------------
instance PGLift Value (Column PGJsonb) where
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


------------------------------------------------------------------------------
type family UnPGScalar (a :: *) :: *
type instance UnPGScalar PGBool = Bool
type instance UnPGScalar PGBytea = ByteString
type instance UnPGScalar PGCitext = (CI Text)
type instance UnPGScalar PGDate = Day
type instance UnPGScalar PGFloat8 = Double
type instance UnPGScalar PGFloat4 = Float
type instance UnPGScalar PGInt2 = Int16
type instance UnPGScalar PGInt4 = Int32
type instance UnPGScalar PGInt8 = Int64
type instance UnPGScalar PGTimestamp = LocalTime
type instance UnPGScalar PGText = Text
type instance UnPGScalar PGTime = TimeOfDay
type instance UnPGScalar PGTimestamptz = UTCTime
type instance UnPGScalar PGUuid = UUID
type instance UnPGScalar PGJsonb = Value


------------------------------------------------------------------------------
type family PGMap (as :: [*]) :: [*] where
    PGMap '[] = '[]
    PGMap (a ': as) = PG a ': PGMap as


------------------------------------------------------------------------------
type family PGMapSnd (as :: [(s, *)]) :: [(s, *)] where
    PGMapSnd '[] = '[]
    PGMapSnd ('(s, a) ': as) = '(s, PG a) ': PGMapSnd as


------------------------------------------------------------------------------
type family UnPGMap (as :: [*]) :: [*] where
    UnPGMap '[] = '[]
    UnPGMap (a ': as) = UnPG a ': UnPGMap as


------------------------------------------------------------------------------
type family UnPGMapSnd (as :: [(s, *)]) :: [(s, *)] where
    UnPGMapSnd '[] = '[]
    UnPGMapSnd ('(s, a) ': as) = '(s, UnPG a) ': UnPGMapSnd as


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
