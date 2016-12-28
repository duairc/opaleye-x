{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.X.Join
    ( Join
    , leftJoin
    , rightJoin
    , fullJoin
    )
where

-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.FunctionalJoin (leftJoinF, rightJoinF, fullJoinF)
import           Opaleye.Internal.Operators (IfPP)
import           Opaleye.Manipulation (Unpackspec)
import           Opaleye.PGTypes (PGBool)
import           Opaleye.QueryArr (Query)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Maybe
                     ( PGMaybe
                     , PGJust, pgJust
                     , PGNothing, pgNothing
                     )


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


------------------------------------------------------------------------------
type Join ls rs =
    ( Default Unpackspec ls ls
    , Default Unpackspec rs rs
    , Default IfPP ls ls
    , Default IfPP rs rs
    , Default IfPP (PGMaybe ls) (PGMaybe ls)
    , Default IfPP (PGMaybe rs) (PGMaybe rs)
    , PGJust ls
    , PGNothing ls
    , PGJust rs
    , PGNothing rs
    )


------------------------------------------------------------------------------
leftJoin :: Join ls rs
    => Query ls
    -> Query rs
    -> (ls -> rs -> Column PGBool)
    -> Query (ls, PGMaybe rs)
leftJoin l r j = leftJoinF ((. pgJust) . (,)) (flip (,) pgNothing) j l r


------------------------------------------------------------------------------
rightJoin :: Join ls rs
    => Query ls
    -> Query rs
    -> (ls -> rs -> Column PGBool)
    -> Query (PGMaybe ls, rs)
rightJoin l r j = rightJoinF ((,) . pgJust) ((,) pgNothing) j l r


------------------------------------------------------------------------------
fullJoin :: Join ls rs
    => Query ls
    -> Query rs
    -> (ls -> rs -> Column PGBool)
    -> Query (PGMaybe ls, PGMaybe rs)
fullJoin l r j = fullJoinF
    ((. pgJust) . (,) . pgJust)
    (flip (,) pgNothing . pgJust)
    ((,) pgNothing . pgJust)
    j l r
