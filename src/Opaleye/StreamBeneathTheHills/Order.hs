{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

module Opaleye.StreamBeneathTheHills.Order
    ( Orderable
    , asc
    , desc
    , ascNullsFirst
    , descNullsLast
    , ordered
    )
where

-- contravariant -------------------------------------------------------------
import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Contravariant.Divisible (divided, conquer)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.Order (Order, PGOrd, orderBy)
import qualified Opaleye.Order as O (asc, desc, ascNullsFirst, descNullsLast)
import           Opaleye.QueryArr (Query, QueryArr)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.TF (PGIn, pg)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
newtype OrderPP a b =
    OrderPP ((forall x. PGOrd x => Order (Column x)) -> Order a)


------------------------------------------------------------------------------
instance Profunctor OrderPP where
    dimap l _ (OrderPP p) = OrderPP (\o -> contramap l (p o))


------------------------------------------------------------------------------
instance ProductProfunctor OrderPP where
    empty = OrderPP $ \_ -> conquer
    OrderPP a ***! OrderPP b = OrderPP (\o -> divided (a o) (b o))


------------------------------------------------------------------------------
instance PGOrd a => Default OrderPP (Column a) (Column a) where
    def = OrderPP (\x -> x)


------------------------------------------------------------------------------
type Orderable a = Default OrderPP a a


------------------------------------------------------------------------------
asc :: forall a b. Orderable b => (a -> b) -> Order a
asc f = contramap f (let OrderPP p = def :: OrderPP b b in p (O.asc id))


------------------------------------------------------------------------------
desc :: forall a b. Orderable b => (a -> b) -> Order a
desc f = contramap f (let OrderPP p = def :: OrderPP b b in p (O.desc id))


------------------------------------------------------------------------------
ascNullsFirst :: forall a b. Orderable b => (a -> b) -> Order a
ascNullsFirst f =
    contramap f (let OrderPP p = def :: OrderPP b b in p (O.ascNullsFirst id))


------------------------------------------------------------------------------
descNullsLast :: forall a b. Orderable b => (a -> b) -> Order a
descNullsLast f =
    contramap f (let OrderPP p = def :: OrderPP b b in p (O.descNullsLast id))


------------------------------------------------------------------------------
ordered :: (PGIn a p, Orderable o) => a -> QueryArr p (o, b) -> Query (o, b)
ordered i = orderBy (asc fst) . lmap (const (pg i))
