{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.X.TF
    ( PG, pg
    , UnPG
    , PGRep
    , PGIn
    , PGOut
    , PGScalar
    , UnPGScalar
    )
where

-- opaleye -------------------------------------------------------------------
import           Opaleye.Constant (Constant, constant)
import           Opaleye.RunQuery (QueryRunner)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Internal


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


------------------------------------------------------------------------------
type PGRep a p = Columns a p


------------------------------------------------------------------------------
type PG a = DistributeColumn a


------------------------------------------------------------------------------
type UnPG p = CollectColumn p


------------------------------------------------------------------------------
type PGIn a p = (PGRep a p, Default Constant a p)


------------------------------------------------------------------------------
type PGOut p a = (PGRep a p, Default QueryRunner p a)


------------------------------------------------------------------------------
pg :: PGIn a p => a -> p
pg = constant
