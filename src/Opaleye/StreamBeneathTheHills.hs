{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.StreamBeneathTheHills
    ( PGRep
    , PG
    , UnPG
    , pg

    , PGScalar
    , UnPGScalar

    , Required
    , required
    , Optional
    , optional
    , Option (Option)

    , MakeTableProperties
    , properties

    , table

    , Orderable
    , ordering
    , ordered

    , In
    , Out

    , query
    , queryFirst
    , insert
    , insertMany
    , insertReturning
    , insertReturningFirst
    , update
    , updateReturning
    , updateReturningFirst

    , LiftRecord
    , liftRecord
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record, Tuple)
import           Data.Labeled (Labeled (Labeled), Field)


-- anonymous-data-product-profunctors ----------------------------------------
import           Data.Anonymous.Profunctor
                     ( ProductAdaptor
                     , pRecord
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const), Alternative)
import           Control.Monad (MonadPlus)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int16, Int32, Int64)
import           Data.Traversable (Traversable)
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup)
#endif
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy (Proxy))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


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
import           Opaleye.RunQuery (QueryRunner)
import           Opaleye.Table (Table (Table), TableProperties)
import qualified Opaleye.Table as O (optional, required)


-- opaleye-trans -------------------------------------------------------------
import           Opaleye.Trans (Transaction)
import qualified Opaleye.Trans as T
                     ( query
                     , queryFirst
                     , insert
                     , insertMany
                     , insertReturning
                     , insertReturningFirst
                     , update
                     , updateReturning
                     , updateReturningFirst
                     )


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default, def)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


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
    PG [a] = Column (PGArray (PGScalar a))
    PG (Maybe a) = Column (Nullable (PGScalar a))
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
pg :: (PGRep a p, Default Constant a p) => a -> p
pg = constant


------------------------------------------------------------------------------
lfmap :: (a -> b) -> Field '(s, a) -> Field '(s, b)
lfmap f (Labeled (Identity a)) = Labeled (Identity (f a))
{-# INLINE lfmap #-}


------------------------------------------------------------------------------
class Required a where
    required'
        :: KnownSymbol s
        => String
        -> Field '(s, TableProperties a a)


------------------------------------------------------------------------------
instance Required (Column a) where
    required' = Labeled . Identity . O.required


------------------------------------------------------------------------------
instance Required a => Required (Identity a) where
    required' = lfmap (dimap (\(Identity a) -> a) Identity) . required'


------------------------------------------------------------------------------
instance Required a => Required (Const a b) where
    required' = lfmap (dimap (\(Const a) -> a) Const) . required'


------------------------------------------------------------------------------
instance Required a => Required (Tagged s a) where
    required' = lfmap (dimap (\(Tagged a) -> a) Tagged) . required'


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


------------------------------------------------------------------------------
class Optional a where
    optional'
        :: KnownSymbol s
        => String
        -> Field '(s, TableProperties (Option a) a)


------------------------------------------------------------------------------
instance Optional (Column a) where
    optional' = Labeled . Identity . lmap (\(Option a) -> a) . O.optional


------------------------------------------------------------------------------
instance Optional a => Optional (Identity a) where
    optional' = lfmap (dimap (fmap (\(Identity a) -> a)) Identity)
        . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Const a b) where
    optional' = lfmap (dimap (fmap (\(Const a) -> a)) Const) . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Tagged s a) where
    optional' = lfmap (dimap (fmap (\(Tagged a) -> a)) Tagged)
        . optional'


------------------------------------------------------------------------------
required
    :: forall s a. (KnownSymbol s, Required a)
    => String
    -> Field '(s, TableProperties a a)
required = required'


------------------------------------------------------------------------------
optional
    :: forall s a. (KnownSymbol s, Optional a)
    => String
    -> Field '(s, TableProperties (Option a) a)
optional = optional'


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
instance (KnownSymbol s, MakeTableProperties abs as bs, Required a) =>
    MakeTableProperties
        ('(s, TableProperties a a) ': abs)
        ('(s, a) ': as)
        ('(s, a) ': bs)
  where
    properties = Cons (required (symbolVal (Proxy :: Proxy s))) properties


------------------------------------------------------------------------------
instance (KnownSymbol s, MakeTableProperties abs as bs, Optional a) =>
    MakeTableProperties
        ('(s, TableProperties (Option a) a) ': abs)
        ('(s, Option a) ': as)
        ('(s, a) ': bs)
  where
    properties = Cons (optional (symbolVal (Proxy :: Proxy s))) properties


------------------------------------------------------------------------------
table
    ::
        ( MakeTableProperties abs as bs
        , ProductAdaptor TableProperties Field abs as bs
        )
    => String
    -> Table (Record as) (Record bs)
table s = Table s $ pRecord properties


------------------------------------------------------------------------------
class LiftRecord r w where
    liftRecord :: Functor f => Product (Labeled f) r -> Product (Labeled f) w


------------------------------------------------------------------------------
instance LiftRecord '[] '[] where
    liftRecord Nil = Nil


------------------------------------------------------------------------------
instance LiftRecord as bs => LiftRecord (a ': as) (a ': bs) where
    liftRecord (Cons a as) = Cons a (liftRecord as)


------------------------------------------------------------------------------
instance (Applicative f, LiftRecord as bs) =>
    LiftRecord ('(s, a) ': as) ('(s, f a) ': bs)
  where
    liftRecord (Cons (Labeled a) as) =
        Cons (Labeled (pure <$> a)) (liftRecord as)


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
instance Orderable (f a) => Orderable (Labeled f '(s, a)) where
    ordering = contramap unlabel ordering
      where
        unlabel :: Labeled f '(s, a) -> f a
        unlabel (Labeled a) = a


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
instance Orderable (Product g '[]) where
    ordering = mempty


------------------------------------------------------------------------------
instance (Orderable (g a), Orderable (Product g as)) =>
    Orderable (Product g (a ': as))
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


------------------------------------------------------------------------------
class (Default Constant a p, PGRep a p) => In a p | a -> p, p -> a
instance (Default Constant a p, PGRep a p) => In a p


------------------------------------------------------------------------------
class (Default QueryRunner p a, PGRep a p) => Out p a | a -> p, p -> a
instance (Default QueryRunner p a, PGRep a p) => Out p a


------------------------------------------------------------------------------
query :: forall b a b' a'. (In a a', Out b' b)
    => QueryArr a' b' -> a -> Transaction [b]
query q a = T.query (lmap (\_ -> pg a) q)


------------------------------------------------------------------------------
queryFirst :: forall b a b' a'. (In a a', Out b' b)
    => QueryArr a' b' -> a -> Transaction (Maybe b)
queryFirst q a = T.queryFirst (lmap (\_ -> pg a) q)


------------------------------------------------------------------------------
insert :: In a w => Table w r -> a -> Transaction Int64
insert t = T.insert t . pg


------------------------------------------------------------------------------
insertMany :: In a w => Table w r -> [a] -> Transaction Int64
insertMany t = T.insertMany t . map pg


------------------------------------------------------------------------------
insertReturning :: (In a w, Out r' b)
    => Table w r
    -> (r -> r')
    -> a
    -> Transaction [b]
insertReturning t f = T.insertReturning t f . pg


------------------------------------------------------------------------------
insertReturningFirst :: (In a w, Out r' b)
    => Table w r
    -> (r -> r')
    -> a
    -> Transaction (Maybe b)
insertReturningFirst t f = T.insertReturningFirst t f . pg


------------------------------------------------------------------------------
update :: LiftRecord r w
    => Table (Record w) (Record r)
    -> (Record w -> Record w)
    -> (Record r -> PG Bool)
    -> Transaction Int64
update t f = T.update t (f . liftRecord)


------------------------------------------------------------------------------
updateReturning :: (Default QueryRunner r' a, LiftRecord r w)
    => Table (Record w) (Record r)
    -> (Record w -> Record w)
    -> (Record r -> PG Bool)
    -> (Record r -> r')
    -> Transaction [a]
updateReturning t f = T.updateReturning t (f . liftRecord)


------------------------------------------------------------------------------
updateReturningFirst :: (Default QueryRunner r' a, LiftRecord r w)
    => Table (Record w) (Record r)
    -> (Record w -> Record w)
    -> (Record r -> PG Bool)
    -> (Record r -> r')
    -> Transaction (Maybe a)
updateReturningFirst t f = T.updateReturningFirst t (f . liftRecord)
