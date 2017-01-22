{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.X.TF
    ( PGRep, PG, UnPG, pg
    , NullRep, Null, UnNull
    , ArrayRep, Array, UnArray
    , PGIn, PGOut
    , PGScalar, UnPGScalar
    )
where

-- opaleye -------------------------------------------------------------------
import           Opaleye.Constant (Constant, constant)
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Internal
import           Opaleye.X.Array ()
import           Opaleye.X.Maybe ()
import           Opaleye.X.Optional ()


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


------------------------------------------------------------------------------
type PGRep a p = Columns a p


------------------------------------------------------------------------------
type PG a = DistributeColumn a


------------------------------------------------------------------------------
type UnPG p = CollectColumn p


------------------------------------------------------------------------------
type NullRep a p = Nullables a p


------------------------------------------------------------------------------
type Null a = DistributeNullable a


------------------------------------------------------------------------------
type UnNull p = CollectNullable p


------------------------------------------------------------------------------
type ArrayRep a p = PGArrays a p


------------------------------------------------------------------------------
type Array a = DistributePGArray a


------------------------------------------------------------------------------
type UnArray p = CollectPGArray p


------------------------------------------------------------------------------
type PGIn a p = (PGRep a p, Default Constant a p)


------------------------------------------------------------------------------
type PGOut p a = (PGRep a p, Default QueryRunner p a)


------------------------------------------------------------------------------
pg :: PGIn a p => a -> p
pg = constant
