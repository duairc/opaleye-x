{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.StreamBeneathTheHills.TF
    ( PG, pg
    , UnPG
    , PGRep
    , PGIn
    , PGOut
    , PGScalar
    , UnPGScalar
    )
where

-- opaleye-of-the-stream-beheath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Internal
                     ( Columns
                     , DistributeColumn
                     , CollectColumn
                     , Constant
                     , Run
                     , PGScalar
                     , UnPGScalar
                     , constant
                     )


------------------------------------------------------------------------------
type PGRep a p = Columns a p


------------------------------------------------------------------------------
type PG a = DistributeColumn a


------------------------------------------------------------------------------
type UnPG p = CollectColumn p


------------------------------------------------------------------------------
type PGIn a p = Constant a p


------------------------------------------------------------------------------
type PGOut p a = Run a p


------------------------------------------------------------------------------
pg :: PGIn a p => a -> p
pg = constant
