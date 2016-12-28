{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#define __OVERLAPS__
#else
#define __OVERLAPS__ {-# OVERLAPS #-}
#endif

module Opaleye.StreamBeneathTheHills.Internal
    ( Columns
    , Constant, constant
    , Run

    , PG
    , UnPG

    , PGArray
    , PGFromList, pgFromList
    , ArrayAgg, arrayAgg

    , PGMaybe
    , PGMatchMaybe, pgMatchMaybe
    , PGNothing, pgNothing
    , PGJust, pgJust
    , PGIsJust, pgIsJust, pgIsNothing

    , Optional
    , MatchOptional, matchOptional
    , Defaults, defaults
    , Override, override
    , Optionify, optionify

    , Table, table, tableWithSchema
    , TableSpec
    , Properties, properties
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record, Tuple)
import           Data.Labeled (Field, Labeled (Labeled))


-- anonymous-data-product-profunctors ----------------------------------------
import           Data.Anonymous.Profunctor ()


-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, Const)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Monad (MonadPlus)
import           Control.Monad.Zip (MonadZip, mzip, munzip)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Foldable (Foldable)
#endif
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32, Int64)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
import           Data.Proxy (Proxy (Proxy))
import           Data.Semigroup (Semigroup, (<>))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (Traversable)
#endif
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Prelude hiding (null)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Aggregate (Aggregator)
import qualified Opaleye.Aggregate as O (arrayAgg)
import           Opaleye.Column
                     ( Column
                     , Nullable
                     , isNull
                     , null
                     , toNullable
                     , unsafeCoerceColumn
                     )
import qualified Opaleye.Constant as O (Constant, constant)
import           Opaleye.Internal.Operators (IfPP)
import           Opaleye.Internal.TableMaker (ColumnMaker)
import           Opaleye.Operators ((.&&), ifThenElseMany)
import qualified Opaleye.Operators as O (not)
import           Opaleye.PGTypes
                     ( IsSqlType
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
                     , pgArray
                     , pgBool
                     )
import qualified Opaleye.PGTypes as O (PGArray)
import           Opaleye.RunQuery (QueryRunner)
import           Opaleye.Table (TableProperties, required, optional)
import qualified Opaleye.Table as O (Table (Table, TableWithSchema))


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import           Data.Profunctor.Product.Default (Default, def)


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
type family PG a :: *
type instance PG Bool = PGBool
type instance PG ByteString = PGBytea
type instance PG (CI Text) = PGCitext
type instance PG Day = PGDate
type instance PG Double = PGFloat8
type instance PG Float = PGFloat4
type instance PG Int16 = PGInt2
type instance PG Int32 = PGInt4
type instance PG Int64 = PGInt8
type instance PG LocalTime = PGTimestamp
type instance PG Text = PGText
type instance PG TimeOfDay = PGTime
type instance PG UTCTime = PGTimestamptz
type instance PG UUID = PGUuid
type instance PG Value = PGJsonb


------------------------------------------------------------------------------
type family UnPG a :: *
type instance UnPG PGBool = Bool
type instance UnPG PGBytea = ByteString
type instance UnPG PGCitext = CI Text
type instance UnPG PGDate = Day
type instance UnPG PGFloat8 = Double
type instance UnPG PGFloat4 = Float
type instance UnPG PGInt2 = Int16
type instance UnPG PGInt4 = Int32
type instance UnPG PGInt8 = Int64
type instance UnPG PGTimestamp = LocalTime
type instance UnPG PGText = Text
type instance UnPG PGTime = TimeOfDay
type instance UnPG PGTimestamptz = UTCTime
type instance UnPG PGUuid = UUID
type instance UnPG PGJsonb = Value


------------------------------------------------------------------------------
type family DistributeColumn a :: * where
    DistributeColumn () = ()
    DistributeColumn (a, b) = (DistributeColumn a, DistributeColumn b)
    DistributeColumn (a, b, c) =
        (DistributeColumn a, DistributeColumn b, DistributeColumn c)
    DistributeColumn (a, b, c, d) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d
        )
    DistributeColumn (a, b, c, d, e) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e
        )
    DistributeColumn (a, b, c, d, e, f) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e, DistributeColumn f
        )
    DistributeColumn (a, b, c, d, e, f, g) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e, DistributeColumn f
        , DistributeColumn g
        )
    DistributeColumn (a, b, c, d, e, f, g, h) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e, DistributeColumn f
        , DistributeColumn g, DistributeColumn h
        )
    DistributeColumn (a, b, c, d, e, f, g, h, i) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e, DistributeColumn f
        , DistributeColumn g, DistributeColumn h, DistributeColumn i
        )
    DistributeColumn (a, b, c, d, e, f, g, h, i, j) =
        ( DistributeColumn a, DistributeColumn b, DistributeColumn c
        , DistributeColumn d, DistributeColumn e, DistributeColumn f
        , DistributeColumn g, DistributeColumn h, DistributeColumn i
        , DistributeColumn j
        )
    DistributeColumn (Tuple as) = Tuple (MapDistributeColumn as)
    DistributeColumn (Record as) = Record (MapSndDistributeColumn as)
    DistributeColumn (Const a b) = Const (DistributeColumn a) b
    DistributeColumn (Identity a) = Identity (DistributeColumn a)
    DistributeColumn (Tagged s a) = Tagged s (DistributeColumn a)
    DistributeColumn (Field '(s, a)) = Field '(s, DistributeColumn a)
    DistributeColumn [a] = PGArray (DistributeColumn a)
    DistributeColumn (Maybe a) = PGMaybe (DistributeColumn a)
    DistributeColumn (Option a) = Option (DistributeColumn a)
    DistributeColumn (Optional a) = Optional (DistributeColumn a)
    DistributeColumn a = Column (PG a)


------------------------------------------------------------------------------
type family MapDistributeColumn (as :: [*]) :: [*] where
    MapDistributeColumn '[] = '[]
    MapDistributeColumn (a ': as) =
        DistributeColumn a ': MapDistributeColumn as


------------------------------------------------------------------------------
type family MapSndDistributeColumn (as :: [(k, *)]) :: [(k, *)] where
    MapSndDistributeColumn '[] = '[]
    MapSndDistributeColumn ('(s, a) ': as) =
        '(s, DistributeColumn a) ': MapSndDistributeColumn as


------------------------------------------------------------------------------
type family CollectColumn a :: * where
    CollectColumn () = ()
    CollectColumn (a, b) = (CollectColumn a, CollectColumn b)
    CollectColumn (a, b, c) =
        (CollectColumn a, CollectColumn b, CollectColumn c)
    CollectColumn (a, b, c, d) =
        (CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d)
    CollectColumn (a, b, c, d, e) =
        (CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e
        )
    CollectColumn (a, b, c, d, e, f) =
        ( CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e, CollectColumn f
        )
    CollectColumn (a, b, c, d, e, f, g) =
        ( CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e, CollectColumn f, CollectColumn g
        )
    CollectColumn (a, b, c, d, e, f, g, h) =
        ( CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e, CollectColumn f, CollectColumn g, CollectColumn h
        )
    CollectColumn (a, b, c, d, e, f, g, h, i) =
        ( CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e, CollectColumn f, CollectColumn g, CollectColumn h
        , CollectColumn i
        )
    CollectColumn (a, b, c, d, e, f, g, h, i, j) =
        ( CollectColumn a, CollectColumn b, CollectColumn c, CollectColumn d
        , CollectColumn e, CollectColumn f, CollectColumn g, CollectColumn h
        , CollectColumn i, CollectColumn j
        )
    CollectColumn (Tuple as) = Tuple (MapCollectColumn as)
    CollectColumn (Record as) = Record (MapSndCollectColumn as)
    CollectColumn (Const a b) = Const (CollectColumn a) b
    CollectColumn (Identity a) = Identity (CollectColumn a)
    CollectColumn (Tagged s a) = Tagged s (CollectColumn a)
    CollectColumn (Field '(s, a)) = Field '(s, CollectColumn a)
    CollectColumn (PGArray a) = [CollectColumn a]
    CollectColumn (PGMaybe a) = Maybe (CollectColumn a)
    CollectColumn (Option a) = Option (CollectColumn a)
    CollectColumn (Optional a) = Optional (CollectColumn a)
    CollectColumn (Column a) = UnPG a


------------------------------------------------------------------------------
type family MapCollectColumn (as :: [*]) :: [*] where
    MapCollectColumn '[] = '[]
    MapCollectColumn (a ': as) = CollectColumn a ': MapCollectColumn as


------------------------------------------------------------------------------
type family MapSndCollectColumn (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectColumn '[] = '[]
    MapSndCollectColumn ('(s, a) ': as) =
        '(s, CollectColumn a) ': MapSndCollectColumn as


------------------------------------------------------------------------------
type Columns a p = (DistributeColumn a ~ p, CollectColumn p ~ a)


------------------------------------------------------------------------------
type Constant a p = (Columns a p, Default O.Constant a p)


------------------------------------------------------------------------------
type Run a p = (Columns a p, Default QueryRunner p a)


------------------------------------------------------------------------------
constant :: Constant a p => a -> p
constant = O.constant


------------------------------------------------------------------------------
newtype PGArray a = PGArray (DistributePGArray a)
  deriving (Generic, Typeable)


------------------------------------------------------------------------------
instance (PGArrays a as, PGArrays b bs, Default p as bs, Profunctor p) =>
    Default p (PGArray a) (PGArray b)
  where
    def = dimap (\(PGArray a) -> a) PGArray def


------------------------------------------------------------------------------
instance (Columns a p, Default O.Constant [a] (Column (O.PGArray p))) =>
    Default (L [] O.Constant) a (Column (O.PGArray p))
  where
    def = L def


------------------------------------------------------------------------------
instance (Columns a p, PGArrays p ps, Default (L [] O.Constant) a ps) =>
    Default O.Constant [a] (PGArray p)
  where
    def = let L p = def in rmap PGArray p


------------------------------------------------------------------------------
instance (Columns a p, Default QueryRunner (Column (O.PGArray p)) [a]) =>
    Default (R [] QueryRunner) (Column (O.PGArray p)) a
  where
    def = R def


------------------------------------------------------------------------------
instance (Columns a p, PGArrays p ps, Default (R [] QueryRunner) ps a) =>
    Default QueryRunner (PGArray p) [a]
  where
    def = let R p = def in lmap (\(PGArray a) -> a) p


------------------------------------------------------------------------------
type family DistributePGArray a :: * where
    DistributePGArray () = ()
    DistributePGArray (a, b) = (DistributePGArray a, DistributePGArray b)
    DistributePGArray (a, b, c) =
        (DistributePGArray a, DistributePGArray b, DistributePGArray c)
    DistributePGArray (a, b, c, d) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d
        )
    DistributePGArray (a, b, c, d, e) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e
        )
    DistributePGArray (a, b, c, d, e, f) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e, DistributePGArray f
        )
    DistributePGArray (a, b, c, d, e, f, g) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e, DistributePGArray f
        , DistributePGArray g
        )
    DistributePGArray (a, b, c, d, e, f, g, h) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e, DistributePGArray f
        , DistributePGArray g, DistributePGArray h
        )
    DistributePGArray (a, b, c, d, e, f, g, h, i) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e, DistributePGArray f
        , DistributePGArray g, DistributePGArray h, DistributePGArray i
        )
    DistributePGArray (a, b, c, d, e, f, g, h, i, j) =
        ( DistributePGArray a, DistributePGArray b, DistributePGArray c
        , DistributePGArray d, DistributePGArray e, DistributePGArray f
        , DistributePGArray g, DistributePGArray h, DistributePGArray i
        , DistributePGArray j
        )
    DistributePGArray (Tuple as) = Tuple (MapDistributePGArray as)
    DistributePGArray (Record as) = Record (MapSndDistributePGArray as)
    DistributePGArray (Const a c) = Const (DistributePGArray a) c
    DistributePGArray (Identity a) = Identity (DistributePGArray a)
    DistributePGArray (Tagged s a) = Tagged s (DistributePGArray a)
    DistributePGArray (Field '(s, a)) = Field '(s, DistributePGArray a)
    DistributePGArray (PGArray a) = PGArray (DistributePGArray a)
    DistributePGArray (PGMaybe a) = PGMaybe (DistributePGArray a)
    DistributePGArray (Option a) = Option (DistributePGArray a)
    DistributePGArray (Optional a) = Optional (DistributePGArray a)
    DistributePGArray (Column a) = Column (O.PGArray a)


------------------------------------------------------------------------------
type family MapDistributePGArray (as :: [*]) :: [*] where
    MapDistributePGArray '[] = '[]
    MapDistributePGArray (a ': as) =
        DistributePGArray a ': MapDistributePGArray as


------------------------------------------------------------------------------
type family MapSndDistributePGArray (as :: [(k, *)]) :: [(k, *)] where
    MapSndDistributePGArray '[] = '[]
    MapSndDistributePGArray ('(s, a) ': as) =
        '(s, DistributePGArray a) ': MapSndDistributePGArray as


------------------------------------------------------------------------------
type family CollectPGArray a :: * where
    CollectPGArray () = ()
    CollectPGArray (a, b) = (CollectPGArray a, CollectPGArray b)
    CollectPGArray (a, b, c) =
        (CollectPGArray a, CollectPGArray b, CollectPGArray c)
    CollectPGArray (a, b, c, d) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d
        )
    CollectPGArray (a, b, c, d, e) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e
        )
    CollectPGArray (a, b, c, d, e, f) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e, CollectPGArray f
        )
    CollectPGArray (a, b, c, d, e, f, g) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e, CollectPGArray f
        , CollectPGArray g
        )
    CollectPGArray (a, b, c, d, e, f, g, h) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e, CollectPGArray f
        , CollectPGArray g, CollectPGArray h
        )
    CollectPGArray (a, b, c, d, e, f, g, h, i) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e, CollectPGArray f
        , CollectPGArray g, CollectPGArray h, CollectPGArray i
        )
    CollectPGArray (a, b, c, d, e, f, g, h, i, j) =
        ( CollectPGArray a, CollectPGArray b, CollectPGArray c
        , CollectPGArray d, CollectPGArray e, CollectPGArray f
        , CollectPGArray g, CollectPGArray h, CollectPGArray i
        , CollectPGArray j
        )
    CollectPGArray (Tuple as) = Tuple (MapCollectPGArray as)
    CollectPGArray (Record as) = Record (MapSndCollectPGArray as)
    CollectPGArray (Const a c) = Const (CollectPGArray a) c
    CollectPGArray (Identity a) = Identity (CollectPGArray a)
    CollectPGArray (Tagged s a) = Tagged s (CollectPGArray a)
    CollectPGArray (Field '(s, a)) = Field '(s, CollectPGArray a)
    CollectPGArray (PGArray a) = PGArray (CollectPGArray a)
    CollectPGArray (PGMaybe a) = PGMaybe (CollectPGArray a)
    CollectPGArray (Option a) = Option (CollectPGArray a)
    CollectPGArray (Optional a) = Optional (CollectPGArray a)
    CollectPGArray (Column (O.PGArray a)) = Column a


------------------------------------------------------------------------------
type family MapCollectPGArray (as :: [*]) :: [*] where
    MapCollectPGArray '[] = '[]
    MapCollectPGArray (a ': as) = CollectPGArray a ': MapCollectPGArray as


------------------------------------------------------------------------------
type family MapSndCollectPGArray (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectPGArray '[] = '[]
    MapSndCollectPGArray ('(s, a) ': as)
        = '(s, CollectPGArray a) ': MapSndCollectPGArray as


------------------------------------------------------------------------------
type PGArrays a as = (DistributePGArray a ~ as, CollectPGArray as ~ a)


------------------------------------------------------------------------------
instance IsSqlType a => Default (L [] (->)) (Column a) (Column (O.PGArray a))
  where
    def = L (pgArray id)


------------------------------------------------------------------------------
type PGFromList a =
    ( PGArrays a (DistributePGArray a)
    , Default (L [] (->)) a (DistributePGArray a)
    )


------------------------------------------------------------------------------
pgFromList :: PGFromList a => [a] -> PGArray a
pgFromList = let L f = def in PGArray . f


------------------------------------------------------------------------------
newtype ArrayAggPP a b = ArrayAggPP (Aggregator a b)
  deriving (Profunctor, ProductProfunctor)


------------------------------------------------------------------------------
instance Default ArrayAggPP (Column a) (Column (O.PGArray a)) where
    def = ArrayAggPP O.arrayAgg


------------------------------------------------------------------------------
type ArrayAgg a =
    ( PGArrays a (DistributePGArray a)
    , Default ArrayAggPP a (DistributePGArray a)
    )


------------------------------------------------------------------------------
arrayAgg :: ArrayAgg a => Aggregator a (PGArray a)
arrayAgg = let ArrayAggPP p = def in rmap PGArray p


------------------------------------------------------------------------------
newtype PGMaybe a = PGMaybe (DistributeNullable a)
  deriving (Generic, Typeable)


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
instance (Nullables a as, Nullables b bs, Default p as bs, Profunctor p) =>
    Default p (PGMaybe a) (PGMaybe b)
  where
    def = dimap (\(PGMaybe a) -> a) PGMaybe def


------------------------------------------------------------------------------
instance (Columns a p, Default O.Constant (Maybe a) (Column (Nullable p))) =>
    Default (L Maybe O.Constant) a (Column (Nullable p))
  where
    def = L def


------------------------------------------------------------------------------
instance (Columns a p, Nullables p ps, Default (L Maybe O.Constant) a ps) =>
    Default O.Constant (Maybe a) (PGMaybe p)
  where
    def = let L p = def in rmap PGMaybe p


------------------------------------------------------------------------------
instance (Columns a p, Default QueryRunner (Column (Nullable p)) (Maybe a)) =>
    Default (R Maybe QueryRunner) (Column (Nullable p)) a
  where
    def = R def


------------------------------------------------------------------------------
instance (Columns a p, Nullables p ps, Default (R Maybe QueryRunner) ps a) =>
    Default QueryRunner (PGMaybe p) (Maybe a)
  where
    def = let R p = def in lmap (\(PGMaybe a) -> a) p


------------------------------------------------------------------------------
type family DistributeNullable a :: * where
    DistributeNullable () = ()
    DistributeNullable (a, b) = (DistributeNullable a, DistributeNullable b)
    DistributeNullable (a, b, c) =
        (DistributeNullable a, DistributeNullable b, DistributeNullable c)
    DistributeNullable (a, b, c, d) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d
        )
    DistributeNullable (a, b, c, d, e) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e
        )
    DistributeNullable (a, b, c, d, e, f) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e, DistributeNullable f
        )
    DistributeNullable (a, b, c, d, e, f, g) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e, DistributeNullable f
        , DistributeNullable g
        )
    DistributeNullable (a, b, c, d, e, f, g, h) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e, DistributeNullable f
        , DistributeNullable g, DistributeNullable h
        )
    DistributeNullable (a, b, c, d, e, f, g, h, i) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e, DistributeNullable f
        , DistributeNullable g, DistributeNullable h, DistributeNullable i
        )
    DistributeNullable (a, b, c, d, e, f, g, h, i, j) =
        ( DistributeNullable a, DistributeNullable b, DistributeNullable c
        , DistributeNullable d, DistributeNullable e, DistributeNullable f
        , DistributeNullable g, DistributeNullable h, DistributeNullable i
        , DistributeNullable j
        )
    DistributeNullable (Tuple as) = Tuple (MapDistributeNullable as)
    DistributeNullable (Record as) = Record (MapSndDistributeNullable as)
    DistributeNullable (Const a c) = Const (DistributeNullable a) c
    DistributeNullable (Identity a) = Identity (DistributeNullable a)
    DistributeNullable (Tagged s a) = Tagged s (DistributeNullable a)
    DistributeNullable (Field '(s, a)) = Field '(s, DistributeNullable a)
    DistributeNullable (PGArray a) = PGArray (DistributeNullable a)
    DistributeNullable (PGMaybe a) = PGMaybe a
    DistributeNullable (Option a) = Option (DistributeNullable a)
    DistributeNullable (Optional a) = Optional (DistributeNullable a)
    DistributeNullable (Column a) = Column (Nullable a)


------------------------------------------------------------------------------
type family MapDistributeNullable (as :: [*]) :: [*] where
    MapDistributeNullable '[] = '[]
    MapDistributeNullable (a ': as) =
        DistributeNullable a ': MapDistributeNullable as


------------------------------------------------------------------------------
type family MapSndDistributeNullable (as :: [(k, *)]) :: [(k, *)] where
    MapSndDistributeNullable '[] = '[]
    MapSndDistributeNullable ('(s, a) ': as) =
        '(s, DistributeNullable a) ': MapSndDistributeNullable as


------------------------------------------------------------------------------
type family CollectNullable a :: * where
    CollectNullable () = ()
    CollectNullable (a, b) = (CollectNullable a, CollectNullable b)
    CollectNullable (a, b, c) =
        (CollectNullable a, CollectNullable b, CollectNullable c)
    CollectNullable (a, b, c, d) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d
        )
    CollectNullable (a, b, c, d, e) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e
        )
    CollectNullable (a, b, c, d, e, f) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e
        )
    CollectNullable (a, b, c, d, e, f, g) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e
        )
    CollectNullable (a, b, c, d, e, f, g, h) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e, CollectNullable f
        , CollectNullable g, CollectNullable h
        )
    CollectNullable (a, b, c, d, e, f, g, h, i) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e, CollectNullable f
        , CollectNullable g, CollectNullable h, CollectNullable i
        )
    CollectNullable (a, b, c, d, e, f, g, h, i, j) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e, CollectNullable f
        , CollectNullable g, CollectNullable h, CollectNullable i
        , CollectNullable j
        )
    CollectNullable (Tuple as) = Tuple (MapCollectNullable as)
    CollectNullable (Record as) = Record (MapSndCollectNullable as)
    CollectNullable (Const a c) = Const (CollectNullable a) c
    CollectNullable (Identity a) = Identity (CollectNullable a)
    CollectNullable (Tagged s a) = Tagged s (CollectNullable a)
    CollectNullable (Field '(s, a)) = Field '(s, CollectNullable a)
    CollectNullable (PGArray a) = PGArray (CollectNullable a)
    CollectNullable (PGMaybe a) = PGMaybe a
    CollectNullable (Option a) = Option (CollectNullable a)
    CollectNullable (Optional a) = Optional (CollectNullable a)
    CollectNullable (Column (Nullable a)) = Column a


------------------------------------------------------------------------------
type family MapCollectNullable (as :: [*]) :: [*] where
    MapCollectNullable '[] = '[]
    MapCollectNullable (a ': as) = CollectNullable a ': MapCollectNullable as


------------------------------------------------------------------------------
type family MapSndCollectNullable (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectNullable '[] = '[]
    MapSndCollectNullable ('(s, a) ': as) =
        '(s, CollectNullable a) ': MapSndCollectNullable as


------------------------------------------------------------------------------
type Nullables a b = (DistributeNullable a ~ b, CollectNullable b ~ a)


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
pgFromJust :: (Nullables a b, Default FromJustPP b a) => PGMaybe a -> a
pgFromJust = let FromJustPP f = def in f . (\(PGMaybe a) -> a)


------------------------------------------------------------------------------
newtype IsJustPP a b = IsJustPP (a -> Column PGBool)


------------------------------------------------------------------------------
instance Profunctor IsJustPP where
    dimap l _ (IsJustPP p) = IsJustPP (lmap l p)


------------------------------------------------------------------------------
instance ProductProfunctor IsJustPP where
    empty = IsJustPP (const (pgBool True))
    IsJustPP a ***! IsJustPP b = IsJustPP (rmap (uncurry (.&&)) (a ***! b))


------------------------------------------------------------------------------
instance Default IsJustPP (Column (Nullable a)) (Column (Nullable a)) where
    def = IsJustPP (O.not . isNull)


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
pgIsNothing = O.not . pgIsJust


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


------------------------------------------------------------------------------
newtype Option a = Option (Maybe a) deriving
    ( Functor, Foldable, Traversable, Applicative, Monad, Alternative
    , MonadPlus, Eq, Ord, Read, Show, Typeable, Generic, Generic1, Monoid
    , Semigroup
    )


------------------------------------------------------------------------------
instance (Default p (Maybe a) (Maybe b), Profunctor p) =>
    Default p (Option a) (Option b)
  where
    def = dimap (\(Option a) -> a) Option def


------------------------------------------------------------------------------
newtype Optional a = Optional (DistributeOption a)
  deriving (Generic, Typeable)


------------------------------------------------------------------------------
instance (Options a oa, Options b ob, Default p oa ob, Profunctor p) =>
    Default p (Optional a) (Optional b)
  where
    def = dimap (\(Optional a) -> a) Optional def


------------------------------------------------------------------------------
type family DistributeOption a :: * where
    DistributeOption () = ()
    DistributeOption (a, b) = (DistributeOption a, DistributeOption b)
    DistributeOption (a, b, c) =
        (DistributeOption a, DistributeOption b, DistributeOption c)
    DistributeOption (a, b, c, d) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d
        )
    DistributeOption (a, b, c, d, e) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e
        )
    DistributeOption (a, b, c, d, e, f) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e, DistributeOption f
        )
    DistributeOption (a, b, c, d, e, f, g) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e, DistributeOption f
        , DistributeOption g
        )
    DistributeOption (a, b, c, d, e, f, g, h) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e, DistributeOption f
        , DistributeOption g, DistributeOption h
        )
    DistributeOption (a, b, c, d, e, f, g, h, i) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e, DistributeOption f
        , DistributeOption g, DistributeOption h, DistributeOption i
        )
    DistributeOption (a, b, c, d, e, f, g, h, i, j) =
        ( DistributeOption a, DistributeOption b, DistributeOption c
        , DistributeOption d, DistributeOption e, DistributeOption f
        , DistributeOption g, DistributeOption h, DistributeOption i
        , DistributeOption j
        )
    DistributeOption (Tuple as) = Tuple (MapDistributeOption as)
    DistributeOption (Record as) = Record (MapSndDistributeOption as)
    DistributeOption (Const a c) = Const (DistributeOption a) c
    DistributeOption (Identity a) = Identity (DistributeOption a)
    DistributeOption (Tagged s a) = Tagged s (DistributeOption a)
    DistributeOption (Field '(s, a)) = Field '(s, DistributeOption a)
    DistributeOption (PGArray a) = PGArray (DistributeOption a)
    DistributeOption [a] = [DistributeOption a]
    DistributeOption (PGMaybe a) = PGMaybe a
    DistributeOption (Maybe a) = Maybe (DistributeOption a)
    DistributeOption a = Option a


------------------------------------------------------------------------------
type family MapDistributeOption (as :: [*]) :: [*] where
    MapDistributeOption '[] = '[]
    MapDistributeOption (a ': as) =
        DistributeOption a ': MapDistributeOption as


------------------------------------------------------------------------------
type family MapSndDistributeOption (as :: [(k, *)]) :: [(k, *)] where
    MapSndDistributeOption '[] = '[]
    MapSndDistributeOption ('(s, a) ': as) =
        '(s, DistributeOption a) ': MapSndDistributeOption as


------------------------------------------------------------------------------
type family CollectOption a :: * where
    CollectOption () = ()
    CollectOption (a, b) = (CollectOption a, CollectOption b)
    CollectOption (a, b, c) =
        (CollectOption a, CollectOption b, CollectOption c)
    CollectOption (a, b, c, d) =
        (CollectOption a, CollectOption b, CollectOption c, CollectOption d)
    CollectOption (a, b, c, d, e) =
        (CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e
        )
    CollectOption (a, b, c, d, e, f) =
        ( CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e, CollectOption f
        )
    CollectOption (a, b, c, d, e, f, g) =
        ( CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e, CollectOption f, CollectOption g
        )
    CollectOption (a, b, c, d, e, f, g, h) =
        ( CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e, CollectOption f, CollectOption g, CollectOption h
        )
    CollectOption (a, b, c, d, e, f, g, h, i) =
        ( CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e, CollectOption f, CollectOption g, CollectOption h
        , CollectOption i
        )
    CollectOption (a, b, c, d, e, f, g, h, i, j) =
        ( CollectOption a, CollectOption b, CollectOption c, CollectOption d
        , CollectOption e, CollectOption f, CollectOption g, CollectOption h
        , CollectOption i, CollectOption j
        )
    CollectOption (Tuple as) = Tuple (MapCollectOption as)
    CollectOption (Record as) = Record (MapSndCollectOption as)
    CollectOption (Const a c) = Const (CollectOption a) c
    CollectOption (Identity a) = Identity (CollectOption a)
    CollectOption (Tagged s a) = Tagged s (CollectOption a)
    CollectOption (Field '(s, a)) = Field '(s, CollectOption a)
    CollectOption (PGArray a) = PGArray (CollectOption a)
    CollectOption [a] = [CollectOption a]
    CollectOption (PGMaybe a) = PGMaybe a
    CollectOption (Maybe a) = Maybe (CollectOption a)
    CollectOption (Option a) = a


------------------------------------------------------------------------------
type family MapCollectOption (as :: [*]) :: [*] where
    MapCollectOption '[] = '[]
    MapCollectOption (a ': as) = CollectOption a ': MapCollectOption as


------------------------------------------------------------------------------
type family MapSndCollectOption (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectOption '[] = '[]
    MapSndCollectOption ( '(s, a) ': as) =
        '(s, CollectOption a) ': MapSndCollectOption as


------------------------------------------------------------------------------
type Options a p = (DistributeOption a ~ p, CollectOption p ~ a)


------------------------------------------------------------------------------
instance Default (R Maybe (->)) (Option a) a where
    def = R (\(Option a) -> a)


------------------------------------------------------------------------------
type MatchOptional a =
    ( Options a (DistributeOption a)
    , Default (R Maybe (->)) (DistributeOption a) a
    )


------------------------------------------------------------------------------
matchOptional :: MatchOptional a => b -> (a -> b) -> Optional a -> b
matchOptional b f (Optional a) = let R p = def in maybe b f (p a)


------------------------------------------------------------------------------
instance Default NothingPP (Option a) (Option a) where
    def = NothingPP (Option Nothing)


------------------------------------------------------------------------------
type Defaults a =
    ( Options a (DistributeOption a)
    , Default NothingPP (DistributeOption a) (DistributeOption a)
    )


------------------------------------------------------------------------------
defaults :: forall a. Defaults a => Optional a
defaults = let NothingPP n = p in Optional n
  where
    p = def :: NothingPP (DistributeOption a) (DistributeOption a)


------------------------------------------------------------------------------
instance Default JustPP a (Option a) where
    def = JustPP (Option . Just)


------------------------------------------------------------------------------
type Override a =
    ( Options a (DistributeOption a)
    , Default JustPP a (DistributeOption a)
    )


------------------------------------------------------------------------------
override :: Override a => a -> Optional a
override = let JustPP f = def in Optional . f


------------------------------------------------------------------------------
newtype PropertiesPP a b =
    PropertiesPP ([String] -> ([String] -> String) -> TableProperties a b)


------------------------------------------------------------------------------
instance Profunctor PropertiesPP where
    dimap l r (PropertiesPP p) = PropertiesPP $ \ns f -> dimap l r (p ns f)


------------------------------------------------------------------------------
instance ProductProfunctor PropertiesPP where
    empty = PropertiesPP $ \_ _ -> empty
    PropertiesPP a ***! PropertiesPP b =
        PropertiesPP $ \ns f -> a ns f ***! b ns f


------------------------------------------------------------------------------
instance Default PropertiesPP (Column a) (Column a) where
    def = PropertiesPP $ \ns f -> required (f ns)


------------------------------------------------------------------------------
instance Default PropertiesPP (Option (Column a)) (Column a) where
    def = PropertiesPP $ \ns f -> lmap (\(Option a) -> a) (optional (f ns))


------------------------------------------------------------------------------
instance __OVERLAPS__ (Default PropertiesPP (f a) (f b), KnownSymbol s) =>
    Default PropertiesPP (Labeled f '(s, a)) (Labeled f '(s, b))
  where
    def = let PropertiesPP p = def in PropertiesPP $ \ns ->
        dimap unlabel Labeled . p (ns ++ [symbolVal (Proxy :: Proxy s)])
      where
        unlabel :: Labeled f '(s, a) -> f a
        unlabel (Labeled a) = a


------------------------------------------------------------------------------
instance (Options a o, Default PropertiesPP o b) =>
    Default PropertiesPP (Optional a) b
  where
    def = let PropertiesPP p = def in PropertiesPP $ \ns f ->
        lmap (\(Optional a) -> a) (p ns f)


------------------------------------------------------------------------------
type Properties = Default PropertiesPP


------------------------------------------------------------------------------
properties :: Properties a b => ([String] -> String) -> TableProperties a b
properties = let PropertiesPP p = def in p []


------------------------------------------------------------------------------
newtype L f p a b = L (p (f a) b)


------------------------------------------------------------------------------
instance (Functor f, Profunctor p) => Profunctor (L f p) where
    dimap l r (L p) = L (dimap (fmap l) r p)


------------------------------------------------------------------------------
instance (Functor f, MonadZip f, ProductProfunctor p) =>
    ProductProfunctor (L f p)
  where
    empty = L (lmap (const ()) empty)
    L a ***! L b = L (lmap munzip (a ***! b))


------------------------------------------------------------------------------
newtype R f p a b = R (p a (f b))


------------------------------------------------------------------------------
instance (Functor f, Profunctor p) => Profunctor (R f p) where
    dimap l r (R p) = R (dimap l (fmap r) p)


------------------------------------------------------------------------------
instance (Functor f, MonadZip f, ProductProfunctor p) =>
    ProductProfunctor (R f p)
  where
    empty = R (rmap return empty)
    R a ***! R b = R (rmap (uncurry mzip) (a ***! b))


------------------------------------------------------------------------------
type Table as = O.Table
    (Record (MapSndDistributeColumn as))
    (Record (MapSndCollectOptional (MapSndDistributeColumn as)))


------------------------------------------------------------------------------
class
    ( rs ~ MapSndCollectOptional ws
    , bs ~ MapSndCollectOptional as
    , ws ~ MapSndDistributeColumn as
    , as ~ MapSndCollectColumn ws
    , rs ~ MapSndDistributeColumn bs
    , bs ~ MapSndCollectColumn rs
    , rs ~ MapSndDistributeColumn (MapSndCollectOptional as)
    , rs ~ MapSndCollectOptional (MapSndDistributeColumn as)
    , bs ~ MapSndDistributeColumn (MapSndCollectOptional ws)
    , bs ~ MapSndCollectOptional (MapSndDistributeColumn ws)
    , Constant (Record as) (Record ws)
    , Constant (Record bs) (Record rs)
    , Optionify as bs
    , Optionify ws rs
    , Default ColumnMaker (Record rs) (Record rs)
    )
  =>
    TableSpec ws rs as bs | ws -> as rs bs, as -> ws rs bs, rs -> bs, bs -> rs


------------------------------------------------------------------------------
instance TableSpec '[] '[] '[] '[]


------------------------------------------------------------------------------
instance
    ( ('(s, r) ': rs) ~ MapSndCollectOptional ('(s, w) ': ws)
    , ('(s, b) ': bs) ~ MapSndCollectOptional ('(s, a) ': as)
    , ('(s, w) ': ws) ~ MapSndDistributeColumn ('(s, a) ': as)
    , ('(s, a) ': as) ~ MapSndCollectColumn ('(s, w) ': ws)
    , ('(s, r) ': rs) ~ MapSndDistributeColumn ('(s, b) ': bs)
    , ('(s, b) ': bs) ~ MapSndCollectColumn ('(s, r) ': rs)
    , ('(s, r) ': rs) ~ MapSndDistributeColumn (MapSndCollectOptional ('(s, a) ': as))
    , ('(s, r) ': rs) ~ MapSndCollectOptional (MapSndDistributeColumn ('(s, a) ': as))
    , ('(s, b) ': bs) ~ MapSndDistributeColumn (MapSndCollectOptional ('(s, w) ': ws))
    , ('(s, b) ': bs) ~ MapSndCollectOptional (MapSndDistributeColumn ('(s, w) ': ws))
    , Constant a w
    , Constant b r
    , Optionify ('(s, a) ': as) ('(s, b) ': bs)
    , Optionify ('(s, w) ': ws) ('(s, r) ': rs)
    , Default ColumnMaker r r
    , KnownSymbol s
    , TableSpec ws rs as bs
    )
  =>
    TableSpec ('(s, w) ': ws) ('(s, r) ': rs) ('(s, a) ': as) ('(s, b) ': bs)


------------------------------------------------------------------------------
table :: (TableSpec ws rs as bs, Properties (Record ws) (Record rs))
    => ([String] -> String) -> String -> Table as
table mangler s = O.Table s (properties mangler)


------------------------------------------------------------------------------
tableWithSchema :: (TableSpec ws rs as bs, Properties (Record ws) (Record rs))
    => ([String] -> String) -> String -> String -> Table as
tableWithSchema mangler n s = O.TableWithSchema n s (properties mangler)


------------------------------------------------------------------------------
type family CollectOptional (a :: *) :: * where
    CollectOptional (Optional a) = a
    CollectOptional a = a


------------------------------------------------------------------------------
type family MapSndCollectOptional (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectOptional '[] = '[]
    MapSndCollectOptional ('(s, a) ': as) =
        '(s, CollectOptional a) ': MapSndCollectOptional as


------------------------------------------------------------------------------
class rs ~ MapSndCollectOptional ws => Optionify ws rs where
    optionify :: Functor f => Product (Labeled f) rs -> Product (Labeled f) ws


------------------------------------------------------------------------------
instance Optionify '[] '[] where
    optionify Nil = Nil


------------------------------------------------------------------------------
instance (Override a, Optionify ws rs) =>
    Optionify ('(s, Optional a) ': ws) ('(s, a) ': rs)
  where
    optionify (Cons (Labeled a) as) =
        Cons (Labeled (fmap override a)) (optionify as)


------------------------------------------------------------------------------
instance (CollectOptional a ~ a, Optionify ws rs) =>
    Optionify ('(s, a) ': ws) ('(s, a) ': rs)
  where
    optionify (Cons a as) = Cons a (optionify as)
