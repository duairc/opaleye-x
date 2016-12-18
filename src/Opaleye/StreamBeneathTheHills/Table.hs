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

module Opaleye.StreamBeneathTheHills.Table
    ( Required
    , required
    , Optional
    , optional

    , Properties (properties)

    , table
    , tableWithSchema
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
import           Opaleye.Table
                     ( Table (Table, TableWithSchema)
                     , TableProperties
                     )
import qualified Opaleye.Table as O (optional, required)


-- opaleye-of-the-stream-beneath-the-hills -----------------------------------
import           Opaleye.StreamBeneathTheHills.Option (Option (Option))


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (dimap, lmap)


-- tagged --------------------------------------------------------------------
import           Data.Tagged (Tagged (Tagged))


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
    properties :: Record abs


------------------------------------------------------------------------------
instance Properties '[] '[] '[] where
    properties = Nil


------------------------------------------------------------------------------
instance (KnownSymbol s, Properties abs as bs, Required a) => Properties
    ('(s, TableProperties a a) ': abs)
    ('(s, a) ': as)
    ('(s, a) ': bs)
  where
    properties = Cons (required (symbolVal (Proxy :: Proxy s))) properties


------------------------------------------------------------------------------
instance (KnownSymbol s, Properties abs as bs, Optional a) => Properties
    ('(s, TableProperties (Option a) a) ': abs)
    ('(s, Option a) ': as)
    ('(s, a) ': bs)
  where
    properties = Cons (optional (symbolVal (Proxy :: Proxy s))) properties


------------------------------------------------------------------------------
table
    :: (Properties abs as bs, ProductAdaptor TableProperties Field abs as bs)
    => String
    -> Table (Record as) (Record bs)
table n = Table n $ pRecord properties


------------------------------------------------------------------------------
tableWithSchema
    :: (Properties abs as bs, ProductAdaptor TableProperties Field abs as bs)
    => String
    -> String
    -> Table (Record as) (Record bs)
tableWithSchema n s = TableWithSchema n s $ pRecord properties
