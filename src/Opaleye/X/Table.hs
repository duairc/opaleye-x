{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

module Opaleye.X.Table
    ( Table, table, tableWithSchema
    , Properties, properties
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Labeled (Labeled (Labeled))


-- base ----------------------------------------------------------------------
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Data.Proxy (Proxy (Proxy))


-- opaleye -------------------------------------------------------------------
import           Opaleye.Column (Column)
import           Opaleye.Table (TableProperties, optional, required)
import qualified Opaleye.Table as O (Table (Table, TableWithSchema))


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.Internal
import           Opaleye.X.TF ()


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap, lmap)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default, def)


------------------------------------------------------------------------------
type Table ws = O.Table ws (CollectOptional ws)


------------------------------------------------------------------------------
table :: (Properties ws rs, Optionals rs ws)
    => ([String] -> String) -> String -> Table ws
table mangler s = O.Table s (properties mangler)


------------------------------------------------------------------------------
tableWithSchema :: (Properties ws rs, Optionals rs ws)
    => ([String] -> String) -> String -> String -> Table ws
tableWithSchema mangler n s = O.TableWithSchema n s (properties mangler)


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
instance (Options a o, Default PropertiesPP o b) =>
    Default PropertiesPP (Optional a) b
  where
    def = let PropertiesPP p = def in PropertiesPP $ \ns f ->
        lmap (\(Optional a) -> a) (p ns f)


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
type Properties = Default PropertiesPP


------------------------------------------------------------------------------
properties :: Properties a b => ([String] -> String) -> TableProperties a b
properties = let PropertiesPP p = def in p []
