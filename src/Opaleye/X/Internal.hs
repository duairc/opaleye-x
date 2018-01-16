{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.X.Internal where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (Value)


-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Record, Tuple)
import           Data.Labeled (Field)


-- anonymous-data-product-profunctors ----------------------------------------
import           Data.Anonymous.Profunctor ()


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const)
import           Control.Monad.Zip (MonadZip, mzip, munzip)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32, Int64)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column, Nullable)
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
                     )
import qualified Opaleye.PGTypes as O (PGArray)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- time ----------------------------------------------------------------------
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (LocalTime, TimeOfDay)


-- uuid-types ---------------------------------------------------------------
import           Data.UUID.Types (UUID)


------------------------------------------------------------------------------
type family PGScalar a :: *
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
type family UnPGScalar a :: *
type instance UnPGScalar PGBool = Bool
type instance UnPGScalar PGBytea = ByteString
type instance UnPGScalar PGCitext = CI Text
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
    DistributeColumn a = Column (PGScalar a)


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
    CollectColumn (Column a) = UnPGScalar a


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
newtype PGArray a = PGArray (DistributePGArray a)


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
    DistributePGArray (Column a) = Column (DistributePGArrayInner a)


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
type family DistributePGArrayInner a :: * where
    DistributePGArrayInner (O.PGArray a) =
        O.PGArray (DistributePGArrayInner a)
    DistributePGArrayInner (Nullable a) =
        Nullable (DistributePGArrayInner a)
    DistributePGArrayInner a = O.PGArray a


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
    CollectPGArray (Column a) = Column (CollectPGArrayInner a)


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
type family CollectPGArrayInner a :: * where
    CollectPGArrayInner (Nullable a) = Nullable (CollectPGArrayInner a)
    CollectPGArrayInner (O.PGArray (Nullable a)) =
        O.PGArray (CollectPGArrayInner (Nullable a))
    CollectPGArrayInner (O.PGArray (O.PGArray a)) =
        O.PGArray (CollectPGArrayInner (O.PGArray a))
    CollectPGArrayInner (O.PGArray a) = a


------------------------------------------------------------------------------
type PGArrays a as = (DistributePGArray a ~ as, CollectPGArray as ~ a)


------------------------------------------------------------------------------
newtype PGMaybe a = PGMaybe (DistributeNullable a)


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
    DistributeNullable (Column a) = Column (DistributeNullableInner a)


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
type family DistributeNullableInner a :: * where
    DistributeNullableInner (Nullable a) =
        Nullable (DistributeNullableInner a)
    DistributeNullableInner (O.PGArray a) =
        O.PGArray (DistributeNullableInner a)
    DistributeNullableInner a = Nullable a


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
        , CollectNullable d, CollectNullable e, CollectNullable f
        )
    CollectNullable (a, b, c, d, e, f, g) =
        ( CollectNullable a, CollectNullable b, CollectNullable c
        , CollectNullable d, CollectNullable e, CollectNullable f
        , CollectNullable g
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
    CollectNullable (Column a) = Column (CollectNullableInner a)


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
type family CollectNullableInner a :: * where
    CollectNullableInner (O.PGArray a) = O.PGArray (CollectNullableInner a)
    CollectNullableInner (Nullable (O.PGArray a)) =
        Nullable (CollectNullableInner (O.PGArray a))
    CollectNullableInner (Nullable (Nullable a)) =
        Nullable (CollectNullableInner (Nullable a))
    CollectNullableInner (Nullable a) = a


------------------------------------------------------------------------------
type Nullables a b = (DistributeNullable a ~ b, CollectNullable b ~ a)


------------------------------------------------------------------------------
newtype Option a = Option (Maybe a)


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
type Options a b = (DistributeOption a ~ b, CollectOption b ~ a)


------------------------------------------------------------------------------
newtype Optional a = Optional (DistributeOption a)


------------------------------------------------------------------------------
type family CollectOptional a :: * where
    CollectOptional () = ()
    CollectOptional (a, b) = (CollectOptional a, CollectOptional b)
    CollectOptional (a, b, c) =
        (CollectOptional a, CollectOptional b, CollectOptional c)
    CollectOptional (a, b, c, d) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d
        )
    CollectOptional (a, b, c, d, e) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e
        )
    CollectOptional (a, b, c, d, e, f) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e, CollectOptional f
        )
    CollectOptional (a, b, c, d, e, f, g) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e, CollectOptional f
        , CollectOptional g
        )
    CollectOptional (a, b, c, d, e, f, g, h) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e, CollectOptional f
        , CollectOptional g, CollectOptional h
        )
    CollectOptional (a, b, c, d, e, f, g, h, i) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e, CollectOptional f
        , CollectOptional g, CollectOptional h, CollectOptional i
        )
    CollectOptional (a, b, c, d, e, f, g, h, i, j) =
        ( CollectOptional a, CollectOptional b, CollectOptional c
        , CollectOptional d, CollectOptional e, CollectOptional f
        , CollectOptional g, CollectOptional h, CollectOptional i
        , CollectOptional j
        )
    CollectOptional (Tuple as) = Tuple (MapCollectOptional as)
    CollectOptional (Record as) = Record (MapSndCollectOptional as)
    CollectOptional (Const a c) = Const (CollectOptional a) c
    CollectOptional (Identity a) = Identity (CollectOptional a)
    CollectOptional (Tagged s a) = Tagged s (CollectOptional a)
    CollectOptional (Field '(s, a)) = Field '(s, CollectOptional a)
    CollectOptional (PGArray a) = PGArray (CollectOptional a)
    CollectOptional [a] = [CollectOptional a]
    CollectOptional (PGMaybe a) = PGMaybe a
    CollectOptional (Maybe a) = Maybe (CollectOptional a)
    CollectOptional (Option a) = Option (CollectOptional a)
    CollectOptional (Optional a) = CollectOptional a
    CollectOptional a = a


------------------------------------------------------------------------------
type family MapCollectOptional (as :: [*]) :: [*] where
    MapCollectOptional '[] = '[]
    MapCollectOptional (a ': as) = CollectOptional a ': MapCollectOptional as


------------------------------------------------------------------------------
type family MapSndCollectOptional (as :: [(k, *)]) :: [(k, *)] where
    MapSndCollectOptional '[] = '[]
    MapSndCollectOptional ('(s, a) ': as) =
        '(s, CollectOptional a) ': MapSndCollectOptional as


------------------------------------------------------------------------------
type Optionals a b = a ~ CollectOptional b


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
