{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.StreamBeneathTheHills.Order (Orderable (ordering), ordered)
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons))
import           Data.Labeled (Labeled (Labeled))


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Monoid ((<>))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (mconcat, mempty)
#endif


-- contravariant -------------------------------------------------------------
import           Data.Functor.Contravariant (contramap)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.Order (Order, PGOrd, asc, orderBy)
import           Opaleye.QueryArr (Query, QueryArr)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.TF (ToPG, pg)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (lmap)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


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
ordered :: (ToPG a p, Orderable o) => a -> QueryArr p (o, b) -> Query (o, b)
ordered i = orderBy (contramap fst ordering) . lmap (const (pg i))
