{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Opaleye.StreamBeneathTheHills
    ( PGRep
    , PG
    , UnPG
    , run
    , pg
    , PGScalar
    , UnPGScalar
    , Table'
    , table
    , Required
    , required
    , Optional
    , optional
    , MakeTableProperties
    , properties
    , Orderable
    , ordering
    , ordered
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-types -----------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record, Tuple)
import           Data.Field (Field (Field))
import           Data.Uncurry (Uncurry (Uncurry), umap)


-- anonymous-types-product-profunctors ---------------------------------------
import           Data.Anonymous.Profunctor
                     ( ProductAdaptor
                     , pRecord
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int16, Int32, Int64)
import           Data.Monoid
                     ( (<>)
#if !MIN_VERSION_base(4, 8, 0)
                     , mconcat
                     , mempty
#endif
                     )


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- contravariant -------------------------------------------------------------
import           Data.Functor.Contravariant (contramap)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable)
import           Opaleye.Constant (Constant, constant)
import           Opaleye.Order (Order, PGOrd, asc, orderBy)
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
import           Opaleye.QueryArr (Query, QueryArr)
import           Opaleye.RunQuery (QueryRunner, runQuery)
import           Opaleye.Table (Table (Table), TableProperties)
import qualified Opaleye.Table as O (optional, required)


-- postgresql-simple ---------------------------------------------------------
import           Database.PostgreSQL.Simple (Connection)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (dimap, lmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT, ask)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- time ----------------------------------------------------------------------
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (LocalTime, TimeOfDay)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat
                     ( KnownSymbol
#ifdef DataPolyKinds
                     , Symbol
#endif
                     , symbolVal
                     )
import           Type.List (Cons, Nil)
import           Type.Meta (Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


-- uuid-types ----------------------------------------------------------------
import           Data.UUID.Types (UUID)


------------------------------------------------------------------------------
class (PG a ~ p, UnPG p ~ a) => PGRep a p | p -> a, a -> p where
    type PG a :: *
    type UnPG p :: *


------------------------------------------------------------------------------
instance PGRep a p => PGRep (Const a b) (Const p b) where
    type PG (Const a b) = Const (PG a) b
    type UnPG (Const p b) = Const (UnPG p) b


------------------------------------------------------------------------------
instance PGRep a p => PGRep (Identity a) (Identity p) where
    type PG (Identity a) = Identity (PG a)
    type UnPG (Identity p) = Identity (UnPG p)


------------------------------------------------------------------------------
instance PGRep a p => PGRep (Tagged s a) (Tagged s p) where
    type PG (Tagged s a) = Tagged s (PG a)
    type UnPG (Tagged s p) = Tagged s (UnPG p)


------------------------------------------------------------------------------
instance PGRep a p => PGRep (Field s a) (Field s p) where
    type PG (Field s a) = Field s (PG a)
    type UnPG (Field s p) = Field s (UnPG p)


------------------------------------------------------------------------------
instance PGRep () () where
    type PG () = ()
    type UnPG () = ()


------------------------------------------------------------------------------
instance (PGRep a pa, PGRep b pb) => PGRep (a, b) (pa, pb) where
    type PG (a, b) = (PG a, PG b)
    type UnPG (pa, pb) = (UnPG pa, UnPG pb)


------------------------------------------------------------------------------
instance (PGRep a pa, PGRep b pb, PGRep c pc) => PGRep (a, b, c) (pa, pb, pc)
  where
    type PG (a, b, c) = (PG a, PG b, PG c)
    type UnPG (pa, pb, pc) = (UnPG pa, UnPG pb, UnPG pc)


------------------------------------------------------------------------------
instance (PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd) =>
    PGRep (a, b, c, d) (pa, pb, pc, pd)
  where
    type PG (a, b, c, d) = (PG a, PG b, PG c, PG d)
    type UnPG (pa, pb, pc, pd) = (UnPG pa, UnPG pb, UnPG pc, UnPG pd)


------------------------------------------------------------------------------
instance (PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe) =>
    PGRep (a, b, c, d, e) (pa, pb, pc, pd, pe)
  where
    type PG (a, b, c, d, e) = (PG a, PG b, PG c, PG d, PG e)
    type UnPG (pa, pb, pc, pd, pe) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe)


------------------------------------------------------------------------------
instance
    (PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe, PGRep f pf)
  =>
    PGRep (a, b, c, d, e, f) (pa, pb, pc, pd, pe, pf)
  where
    type PG (a, b, c, d, e, f) = (PG a, PG b, PG c, PG d, PG e, PG f)
    type UnPG (pa, pb, pc, pd, pe, pf) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf)


------------------------------------------------------------------------------
instance
    ( PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe, PGRep f pf
    , PGRep g pg
    )
  =>
    PGRep (a, b, c, d, e, f, g) (pa, pb, pc, pd, pe, pf, pg)
  where
    type PG (a, b, c, d, e, f, g) = (PG a, PG b, PG c, PG d, PG e, PG f, PG g)
    type UnPG (pa, pb, pc, pd, pe, pf, pg) =
        (UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg)


------------------------------------------------------------------------------
instance
    ( PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe, PGRep f pf
    , PGRep g pg, PGRep h ph
    )
  =>
    PGRep (a, b, c, d, e, f, g, h) (pa, pb, pc, pd, pe, pf, pg, ph)
  where
    type PG (a, b, c, d, e, f, g, h) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h)
    type UnPG (pa, pb, pc, pd, pe, pf, pg, ph) =
        ( UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg
        , UnPG ph
        )


------------------------------------------------------------------------------
instance
    ( PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe, PGRep f pf
    , PGRep g pg, PGRep h ph, PGRep i pi
    )
  =>
    PGRep (a, b, c, d, e, f, g, h, i) (pa, pb, pc, pd, pe, pf, pg, ph, pi)
  where
    type PG (a, b, c, d, e, f, g, h, i) =
        (PG a, PG b, PG c, PG d, PG e, PG f, PG g, PG h, PG i)
    type UnPG (pa, pb, pc, pd, pe, pf, pg, ph, pi) =
        ( UnPG pa, UnPG pb, UnPG pc, UnPG pd, UnPG pe, UnPG pf, UnPG pg
        , UnPG ph, UnPG pi
        )


------------------------------------------------------------------------------
instance
    ( PGRep a pa, PGRep b pb, PGRep c pc, PGRep d pd, PGRep e pe, PGRep f pf
    , PGRep g pg, PGRep h ph, PGRep i pi, PGRep j pj
    )
  =>
    PGRep
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
instance PGRep (Product g Nil) (Product g Nil) where
    type PG (Product g Nil) = Product g Nil
    type UnPG (Product g Nil) = Product g Nil


------------------------------------------------------------------------------
instance
    ( PGRep a p
    , PGRep (Tuple as) (Tuple ps)
    , PG (Tuple as) ~ Tuple (PGMap as)
    , UnPG (Tuple ps) ~ Tuple (UnPGMap ps)
    )
  =>
    PGRep (Tuple (Cons a as)) (Tuple (Cons p ps))
  where
    type PG (Tuple (Cons a as)) = Tuple (Cons (PG a) (PGMap as))
    type UnPG (Tuple (Cons p ps)) = Tuple (Cons (UnPG p) (UnPGMap ps))


------------------------------------------------------------------------------
instance
    ( PGRep a p
    , PGRep (Record as) (Record ps)
    , PG (Record as) ~ Record (PGMapSnd as)
    , UnPG (Record ps) ~ Record (UnPGMapSnd ps)
    )
  =>
    PGRep (Record (Cons (Pair s a) as)) (Record (Cons (Pair s p) ps))
  where
    type PG (Record (Cons (Pair s a) as)) =
        Record (Cons (Pair s (PG a)) (PGMapSnd as))
    type UnPG (Record (Cons (Pair s p) ps)) =
        Record (Cons (Pair s (UnPG p)) (UnPGMapSnd ps))


------------------------------------------------------------------------------
instance
    ( PGRep a (Column p)
    , p ~ PGScalar a
    , a ~ UnPGScalar p
    )
  =>
    PGRep (Maybe a) (Column (Nullable p))
  where
    type PG (Maybe a) = Column (Nullable (PGScalar a))
    type UnPG (Column (Nullable p)) = Maybe (UnPGScalar p)


------------------------------------------------------------------------------
instance
    ( PGRep a (Column p)
    , p ~ PGScalar a
    , a ~ UnPGScalar p
    )
  =>
    PGRep [a] (Column (PGArray p))
  where
    type PG [a] = Column (PGArray (PGScalar a))
    type UnPG (Column (PGArray p)) = [UnPGScalar p]


------------------------------------------------------------------------------
instance PGRep Bool (Column PGBool) where
    type PG Bool = Column PGBool
    type UnPG (Column PGBool) = Bool


------------------------------------------------------------------------------
instance PGRep ByteString (Column PGBytea) where
    type PG ByteString = Column PGBytea
    type UnPG (Column PGBytea) = ByteString


------------------------------------------------------------------------------
instance PGRep (CI Text) (Column PGCitext) where
    type PG (CI Text) = Column PGCitext
    type UnPG (Column PGCitext) = CI Text


------------------------------------------------------------------------------
instance PGRep Day (Column PGDate) where
    type PG Day = Column PGDate
    type UnPG (Column PGDate) = Day


------------------------------------------------------------------------------
instance PGRep Double (Column PGFloat8) where
    type PG Double = Column PGFloat8
    type UnPG (Column PGFloat8) = Double


------------------------------------------------------------------------------
instance PGRep Float (Column PGFloat4) where
    type PG Float = Column PGFloat4
    type UnPG (Column PGFloat4) = Float


------------------------------------------------------------------------------
instance PGRep Int16 (Column PGInt2) where
    type PG Int16 = Column PGInt2
    type UnPG (Column PGInt2) = Int16


------------------------------------------------------------------------------
instance PGRep Int32 (Column PGInt4) where
    type PG Int32 = Column PGInt4
    type UnPG (Column PGInt4) = Int32


------------------------------------------------------------------------------
instance PGRep Int64 (Column PGInt8) where
    type PG Int64 = Column PGInt8
    type UnPG (Column PGInt8) = Int64


------------------------------------------------------------------------------
instance PGRep LocalTime (Column PGTimestamp) where
    type PG LocalTime = Column PGTimestamp
    type UnPG (Column PGTimestamp) = LocalTime


------------------------------------------------------------------------------
instance PGRep Text (Column PGText) where
    type PG Text = Column PGText
    type UnPG (Column PGText) = Text


------------------------------------------------------------------------------
instance PGRep TimeOfDay (Column PGTime) where
    type PG TimeOfDay = Column PGTime
    type UnPG (Column PGTime) = TimeOfDay


------------------------------------------------------------------------------
instance PGRep UUID (Column PGUuid) where
    type PG UUID = Column PGUuid
    type UnPG (Column PGUuid) = UUID


------------------------------------------------------------------------------
instance PGRep UTCTime (Column PGTimestamptz) where
    type PG UTCTime = Column PGTimestamptz
    type UnPG (Column PGTimestamptz) = UTCTime


------------------------------------------------------------------------------
instance PGRep Value (Column PGJsonb) where
    type PG Value = Column PGJsonb
    type UnPG (Column PGJsonb) = Value


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
type family PGMap (as :: KList (*)) :: KList (*)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    PGMap Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    PGMap (Cons a as) = Cons (PG a) (PGMap as)


------------------------------------------------------------------------------
type family PGMapSnd (as :: KList (KPair (KPoly1, *)))
    :: KList (KPair (KPoly1, *))
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    PGMapSnd Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    PGMapSnd (Cons (Pair s a) as) = Cons (Pair s (PG a)) (PGMapSnd as)


------------------------------------------------------------------------------
type family UnPGMap (as :: KList (*)) :: KList (*)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    UnPGMap Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    UnPGMap (Cons a as) = Cons (UnPG a) (UnPGMap as)


------------------------------------------------------------------------------
type family UnPGMapSnd (as :: KList (KPair (KPoly1, *)))
    :: KList (KPair (KPoly1, *))
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    UnPGMapSnd Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    UnPGMapSnd (Cons (Pair s a) as) = Cons (Pair s (UnPG a)) (UnPGMapSnd as)


------------------------------------------------------------------------------
run
    ::
        ( Default Constant ai pi
        , Default QueryRunner po ao
        , PGRep ai pi
        , PGRep ao po
        )
    => ai
    -> QueryArr pi po
    -> ReaderT Connection IO [ao]
run i query = ask >>= \connection -> liftIO (runQuery connection
    (lmap (\_ -> pg i) query))


------------------------------------------------------------------------------
pg :: (PGRep a p, Default Constant a p) => a -> p
pg = constant


------------------------------------------------------------------------------
class Required a where
    required'
        :: KnownSymbol s
        => String
        -> Uncurry Field (Pair s (TableProperties a a))


------------------------------------------------------------------------------
instance Required (Column a) where
    required' = Uncurry . Field . O.required


------------------------------------------------------------------------------
instance Required a => Required (Identity a) where
    required' = umap (fmap (dimap (\(Identity a) -> a) Identity)) . required'


------------------------------------------------------------------------------
instance Required a => Required (Const a b) where
    required' = umap (fmap (dimap (\(Const a) -> a) Const)) . required'


------------------------------------------------------------------------------
instance Required a => Required (Tagged s a) where
    required' = umap (fmap (dimap (\(Tagged a) -> a) Tagged)) . required'


------------------------------------------------------------------------------
class Optional a where
    optional'
        :: KnownSymbol s
        => String
        -> Uncurry Field (Pair s (TableProperties (Maybe a) a))


------------------------------------------------------------------------------
instance Optional (Column a) where
    optional' = Uncurry . Field . O.optional


------------------------------------------------------------------------------
instance Optional a => Optional (Identity a) where
    optional' = umap (fmap (dimap (fmap (\(Identity a) -> a)) Identity))
        . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Const a b) where
    optional' = umap (fmap (dimap (fmap (\(Const a) -> a)) Const)) . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Tagged s a) where
    optional' = umap (fmap (dimap (fmap (\(Tagged a) -> a)) Tagged))
        . optional'


------------------------------------------------------------------------------
required
    :: forall s a. (KnownSymbol s, Required a)
    => String
    -> Uncurry Field (Pair s (TableProperties a a))
required = required'


------------------------------------------------------------------------------
optional
    :: forall s a. (KnownSymbol s, Optional a)
    => String
    -> Uncurry Field (Pair s (TableProperties (Maybe a) a))
optional = optional'


------------------------------------------------------------------------------
class MakeTableProperties
    (abs :: KList (KPair (KString, *)))
    (as :: KList (KPair (KString, *)))
    (bs :: KList (KPair (KString, *)))
        | abs -> as
        , abs -> bs
  where
    properties :: Record abs


------------------------------------------------------------------------------
instance MakeTableProperties Nil Nil Nil where
    properties = Nil


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs, Required a) =>
    MakeTableProperties
        (Cons (Pair s (TableProperties a a)) abs)
        (Cons (Pair s a) as)
        (Cons (Pair s a) bs)
  where
    properties = Cons (required (symbolVal (Proxy :: Proxy s))) properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs, Optional a) =>
    MakeTableProperties
        (Cons (Pair s (TableProperties (Maybe a) a)) abs)
        (Cons (Pair s (Maybe a)) as)
        (Cons (Pair s a) bs)
  where
    properties = Cons (optional (symbolVal (Proxy :: Proxy s))) properties


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


------------------------------------------------------------------------------
class Orderable a where
    ordering :: Order a


------------------------------------------------------------------------------
instance PGOrd a => Orderable (Column a) where
    ordering = asc id


------------------------------------------------------------------------------
instance Orderable a => Orderable (Identity a) where
    ordering = contramap (\(Identity a) -> a) ordering


------------------------------------------------------------------------------
instance Orderable a => Orderable (Const a b) where
    ordering = contramap (\(Const a) -> a) ordering


------------------------------------------------------------------------------
instance Orderable a => Orderable (Tagged s a) where
    ordering = contramap (\(Tagged a) -> a) ordering


------------------------------------------------------------------------------
instance Orderable a => Orderable (Field s a) where
    ordering = contramap (\(Field a) -> a) ordering


------------------------------------------------------------------------------
instance Orderable (f a b) => Orderable (Uncurry f (Pair a b)) where
    ordering = contramap (\(Uncurry f) -> f) (ordering :: Order (f a b))


------------------------------------------------------------------------------
instance Orderable () where
    ordering = mempty


------------------------------------------------------------------------------
instance (Orderable a, Orderable b) => Orderable (a, b) where
    ordering = mconcat
        [ contramap fst ordering
        , contramap snd ordering
        ]


------------------------------------------------------------------------------
instance (Orderable a, Orderable b, Orderable c) => Orderable (a, b, c) where
    ordering = mconcat
        [ contramap fst3 ordering
        , contramap snd3 ordering
        , contramap trd3 ordering
        ]
      where
        fst3 (a, _, _) = a
        snd3 (_, b, _) = b
        trd3 (_, _, c) = c


------------------------------------------------------------------------------
instance (Orderable a, Orderable b, Orderable c, Orderable d) =>
    Orderable (a, b, c, d)
  where
    ordering = mconcat
        [ contramap fst4 ordering
        , contramap snd4 ordering
        , contramap trd4 ordering
        , contramap frt4 ordering
        ]
      where
        fst4 (a, _, _, _) = a
        snd4 (_, b, _, _) = b
        trd4 (_, _, c, _) = c
        frt4 (_, _, _, d) = d


------------------------------------------------------------------------------
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e) =>
    Orderable (a, b, c, d, e)
  where
    ordering = mconcat
        [ contramap fst5 ordering
        , contramap snd5 ordering
        , contramap trd5 ordering
        , contramap frt5 ordering
        , contramap fft5 ordering
        ]
      where
        fst5 (a, _, _, _, _) = a
        snd5 (_, b, _, _, _) = b
        trd5 (_, _, c, _, _) = c
        frt5 (_, _, _, d, _) = d
        fft5 (_, _, _, _, e) = e


------------------------------------------------------------------------------
instance
    ( Orderable a
    , Orderable b
    , Orderable c
    , Orderable d
    , Orderable e
    , Orderable f
    )
  =>
    Orderable (a, b, c, d, e, f)
  where
    ordering = mconcat
        [ contramap fst6 ordering
        , contramap snd6 ordering
        , contramap trd6 ordering
        , contramap frt6 ordering
        , contramap fft6 ordering
        , contramap sxt6 ordering
        ]
      where
        fst6 (a, _, _, _, _, _) = a
        snd6 (_, b, _, _, _, _) = b
        trd6 (_, _, c, _, _, _) = c
        frt6 (_, _, _, d, _, _) = d
        fft6 (_, _, _, _, e, _) = e
        sxt6 (_, _, _, _, _, f) = f


------------------------------------------------------------------------------
instance
    ( Orderable a
    , Orderable b
    , Orderable c
    , Orderable d
    , Orderable e
    , Orderable f
    , Orderable g
    )
  =>
    Orderable (a, b, c, d, e, f, g)
  where
    ordering = mconcat
        [ contramap fst7 ordering
        , contramap snd7 ordering
        , contramap trd7 ordering
        , contramap frt7 ordering
        , contramap fft7 ordering
        , contramap sxt7 ordering
        , contramap svn7 ordering
        ]
      where
        fst7 (a, _, _, _, _, _, _) = a
        snd7 (_, b, _, _, _, _, _) = b
        trd7 (_, _, c, _, _, _, _) = c
        frt7 (_, _, _, d, _, _, _) = d
        fft7 (_, _, _, _, e, _, _) = e
        sxt7 (_, _, _, _, _, f, _) = f
        svn7 (_, _, _, _, _, _, g) = g


------------------------------------------------------------------------------
instance
    ( Orderable a
    , Orderable b
    , Orderable c
    , Orderable d
    , Orderable e
    , Orderable f
    , Orderable g
    , Orderable h
    )
  =>
    Orderable (a, b, c, d, e, f, g, h)
  where
    ordering = mconcat
        [ contramap fst8 ordering
        , contramap snd8 ordering
        , contramap trd8 ordering
        , contramap frt8 ordering
        , contramap fft8 ordering
        , contramap sxt8 ordering
        , contramap svn8 ordering
        , contramap egt8 ordering
        ]
      where
        fst8 (a, _, _, _, _, _, _, _) = a
        snd8 (_, b, _, _, _, _, _, _) = b
        trd8 (_, _, c, _, _, _, _, _) = c
        frt8 (_, _, _, d, _, _, _, _) = d
        fft8 (_, _, _, _, e, _, _, _) = e
        sxt8 (_, _, _, _, _, f, _, _) = f
        svn8 (_, _, _, _, _, _, g, _) = g
        egt8 (_, _, _, _, _, _, _, h) = h


------------------------------------------------------------------------------
instance
    ( Orderable a
    , Orderable b
    , Orderable c
    , Orderable d
    , Orderable e
    , Orderable f
    , Orderable g
    , Orderable h
    , Orderable i
    )
  =>
    Orderable (a, b, c, d, e, f, g, h, i)
  where
    ordering = mconcat
        [ contramap fst9 ordering
        , contramap snd9 ordering
        , contramap trd9 ordering
        , contramap frt9 ordering
        , contramap fft9 ordering
        , contramap sxt9 ordering
        , contramap svn9 ordering
        , contramap egt9 ordering
        , contramap nnt9 ordering
        ]
      where
        fst9 (a, _, _, _, _, _, _, _, _) = a
        snd9 (_, b, _, _, _, _, _, _, _) = b
        trd9 (_, _, c, _, _, _, _, _, _) = c
        frt9 (_, _, _, d, _, _, _, _, _) = d
        fft9 (_, _, _, _, e, _, _, _, _) = e
        sxt9 (_, _, _, _, _, f, _, _, _) = f
        svn9 (_, _, _, _, _, _, g, _, _) = g
        egt9 (_, _, _, _, _, _, _, h, _) = h
        nnt9 (_, _, _, _, _, _, _, _, i) = i


------------------------------------------------------------------------------
instance
    ( Orderable a
    , Orderable b
    , Orderable c
    , Orderable d
    , Orderable e
    , Orderable f
    , Orderable g
    , Orderable h
    , Orderable i
    , Orderable j
    )
  =>
    Orderable (a, b, c, d, e, f, g, h, i, j)
  where
    ordering = mconcat
        [ contramap fst10 ordering
        , contramap snd10 ordering
        , contramap trd10 ordering
        , contramap frt10 ordering
        , contramap fft10 ordering
        , contramap sxt10 ordering
        , contramap svn10 ordering
        , contramap egt10 ordering
        , contramap nnt10 ordering
        , contramap tnt10 ordering
        ]
      where
        fst10 (a, _, _, _, _, _, _, _, _, _) = a
        snd10 (_, b, _, _, _, _, _, _, _, _) = b
        trd10 (_, _, c, _, _, _, _, _, _, _) = c
        frt10 (_, _, _, d, _, _, _, _, _, _) = d
        fft10 (_, _, _, _, e, _, _, _, _, _) = e
        sxt10 (_, _, _, _, _, f, _, _, _, _) = f
        svn10 (_, _, _, _, _, _, g, _, _, _) = g
        egt10 (_, _, _, _, _, _, _, h, _, _) = h
        nnt10 (_, _, _, _, _, _, _, _, i, _) = i
        tnt10 (_, _, _, _, _, _, _, _, _, j) = j


------------------------------------------------------------------------------
instance Orderable (Product g Nil) where
    ordering = mempty


------------------------------------------------------------------------------
instance (Orderable (g a), Orderable (Product g as)) =>
    Orderable (Product g (Cons a as))
  where
    ordering = contramap (\(Cons a _) -> a) (ordering :: Order (g a)) <>
        contramap (\(Cons _ as) -> as) (ordering :: Order (Product g as))



------------------------------------------------------------------------------
ordered
    :: (Default Constant ai pi, PGRep ai pi, Orderable a)
    => ai
    -> QueryArr pi (a, b)
    -> Query (a, b)
ordered i = orderBy (contramap fst ordering) . lmap (const (pg i))
