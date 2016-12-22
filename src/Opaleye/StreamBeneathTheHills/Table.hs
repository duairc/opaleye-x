{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 710
#define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
#else
#define __OVERLAPPABLE__
{-# LANGUAGE OverlappingInstances #-}
#endif

module Opaleye.StreamBeneathTheHills.Table
    ( Table
    , table
    , tableWithSchema

    , Required
    , required
    , Optional
    , optional

    , TableArgs

    , Deoptionify
    , optionify
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record)
import           Data.Labeled (Labeled (Labeled), Field)


-- anonymous-data-product-profunctors ----------------------------------------
import           Data.Anonymous.Profunctor (ProductAdaptor, pRecord)


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Proxy (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.Table (TableProperties)
import qualified Opaleye.Table as O
                     ( Table (Table, TableWithSchema)
                     , optional
                     , required
                     )
import           Opaleye.Internal.TableMaker (ColumnMaker)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Option (Option (Option))
import           Opaleye.StreamBeneathTheHills.TF
                     ( ToPG
                     , FromPG
                     , PGMapSnd
                     , UnPGMapSnd
                     )


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (dimap, lmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product.Default (Default)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


------------------------------------------------------------------------------
class
    ( ws ~ PGMapSnd as
    , as ~ UnPGMapSnd ws
    , rs ~ Deoptionify ws
    , rs ~ PGMapSnd bs
    , rs ~ Deoptionify (PGMapSnd as)
    , bs ~ UnPGMapSnd rs
    , bs ~ Deoptionify as
    , bs ~ Deoptionify (UnPGMapSnd ws)
    , ToPG (Record as) (Record ws)
    , FromPG (Record ws) (Record as)
    , ToPG (Record bs) (Record rs)
    , FromPG (Record rs) (Record bs)
    , Optionify as bs
    , Optionify ws rs
    , Default ColumnMaker (Record rs) (Record rs)
    )
  =>
    TableArgs
        (ws :: [(Symbol, *)])
        (rs :: [(Symbol, *)])
        (as :: [(Symbol, *)])
        (bs :: [(Symbol, *)])
            | ws -> as rs bs
            , as -> ws rs bs
            , rs -> bs
            , bs -> rs


------------------------------------------------------------------------------
instance TableArgs '[] '[] '[] '[]


------------------------------------------------------------------------------
instance
    ( ToPG a w
    , FromPG w a
    , ToPG b r
    , FromPG r b
    , r ~ Deoption w
    , b ~ Deoption a
    , KnownSymbol s
    , Optionify ('(s, a) ': as) ('(s, b) ': bs)
    , Optionify ('(s, w) ': ws) ('(s, r) ': rs)
    , TableArgs ws rs as bs
    , Default ColumnMaker r r
    )
  =>
    TableArgs
        ('(s, w) ': ws)
        ('(s, r) ': rs)
        ('(s, a) ': as)
        ('(s, b) ': bs)


------------------------------------------------------------------------------
type Table as =
    O.Table (Record (PGMapSnd as)) (Record (Deoptionify (PGMapSnd as)))


------------------------------------------------------------------------------
table
    ::
        ( Properties wrs ws rs
        , ProductAdaptor TableProperties Field wrs ws rs
        , TableArgs ws rs as bs
        )
    => (String -> String) -> String -> Table as
table f n = O.Table n $ pRecord (properties f)


------------------------------------------------------------------------------
tableWithSchema
    ::
        ( Properties wrs ws rs
        , ProductAdaptor TableProperties Field wrs ws rs
        , TableArgs ws rs as bs
        )
    => (String -> String) -> String -> String -> Table as
tableWithSchema f n s = O.TableWithSchema n s $ pRecord (properties f)


------------------------------------------------------------------------------
lfmap :: (a -> b) -> Field '(s, a) -> Field '(s, b)
lfmap f (Labeled (Identity a)) = Labeled (Identity (f a))
{-# INLINE lfmap #-}


------------------------------------------------------------------------------
class Required a where
    required' :: KnownSymbol s => String -> Field '(s, TableProperties a a)


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
class Optional a where
    optional' :: KnownSymbol s
        => String -> Field '(s, TableProperties (Option a) a)


------------------------------------------------------------------------------
instance Optional (Column a) where
    optional' = Labeled . Identity . lmap (\(Option a) -> a) . O.optional


------------------------------------------------------------------------------
instance Optional a => Optional (Identity a) where
    optional' = lfmap (dimap (fmap (\(Identity a) -> a)) Identity) . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Const a b) where
    optional' = lfmap (dimap (fmap (\(Const a) -> a)) Const) . optional'


------------------------------------------------------------------------------
instance Optional a => Optional (Tagged s a) where
    optional' = lfmap (dimap (fmap (\(Tagged a) -> a)) Tagged) . optional'


------------------------------------------------------------------------------
required :: forall s a. (KnownSymbol s, Required a)
    => String
    -> Field '(s, TableProperties a a)
required = required'


------------------------------------------------------------------------------
optional :: forall s a. (KnownSymbol s, Optional a)
    => String
    -> Field '(s, TableProperties (Option a) a)
optional = optional'


------------------------------------------------------------------------------
class Properties
    (abs :: [(Symbol, *)])
    (as :: [(Symbol, *)])
    (bs :: [(Symbol, *)])
        | abs -> as
        , abs -> bs
  where
    properties :: (String -> String) -> Record abs


------------------------------------------------------------------------------
instance Properties '[] '[] '[] where
    properties _ = Nil


------------------------------------------------------------------------------
instance (KnownSymbol s, Properties abs as bs, Required a) => Properties
    ('(s, TableProperties a a) ': abs)
    ('(s, a) ': as)
    ('(s, a) ': bs)
  where
    properties f =
        Cons (required (f (symbolVal (Proxy :: Proxy s)))) (properties f)


------------------------------------------------------------------------------
instance (KnownSymbol s, Properties abs as bs, Optional a) => Properties
    ('(s, TableProperties (Option a) a) ': abs)
    ('(s, Option a) ': as)
    ('(s, a) ': bs)
  where
    properties f =
        Cons (optional (f (symbolVal (Proxy :: Proxy s)))) (properties f)


------------------------------------------------------------------------------
type family Deoption (a :: *) :: * where
    Deoption (Option a) = a
    Deoption a = a


------------------------------------------------------------------------------
type family Deoptionify (as :: [(k, *)]) :: [(k, *)] where
    Deoptionify '[] = '[]
    Deoptionify ('(s, a) ': as) = '(s, Deoption a) ': Deoptionify as


------------------------------------------------------------------------------
class rs ~ Deoptionify ws => Optionify ws rs where
    optionify :: Functor f => Product (Labeled f) rs -> Product (Labeled f) ws


------------------------------------------------------------------------------
instance Optionify '[] '[] where
    optionify Nil = Nil


------------------------------------------------------------------------------
instance Optionify ws rs => Optionify ('(s, Option a) ': ws) ('(s, a) ': rs)
  where
    optionify (Cons (Labeled a) as) =
        Cons (Labeled (pure <$> a)) (optionify as)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Deoption a ~ a, Optionify ws rs) =>
    Optionify ('(s, a) ': ws) ('(s, a) ': rs)
  where
    optionify (Cons a as) = Cons a (optionify as)
