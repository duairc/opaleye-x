{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.X.Select
    ( SelectParseError (SelectParseError)
    , Select, selected, selecting, parse
    , select
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Control.Exception (Exception)
import           Control.Monad ((>=>))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)


-- layers --------------------------------------------------------------------
import           Monad.Throw (throw)


-- opaleye -------------------------------------------------------------------
import           Opaleye.RunQuery (QueryRunner)
import           Opaleye.QueryArr (Query)


-- opaleye-x -----------------------------------------------------------------
import           Opaleye.X.TF (PGOut)
import           Opaleye.X.Transaction (Transaction, Run)


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, Star (Star))
import           Data.Profunctor.Composition (Procompose (Procompose))


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, purePP, (****))
import           Data.Profunctor.Product (SumProfunctor)
import           Data.Profunctor.Product.Default (def)


------------------------------------------------------------------------------
newtype SelectParseError = SelectParseError String
  deriving (Eq, Ord, Read, Show, Generic, Typeable)
instance Exception SelectParseError


------------------------------------------------------------------------------
newtype Select p a =
    Select (Procompose (Star (Either String)) QueryRunner p a)
  deriving
    ( Functor, Profunctor, ProductProfunctor, SumProfunctor
    , Generic, Generic1, Typeable
    )


------------------------------------------------------------------------------
instance Applicative (Select a) where
    pure = purePP
    (<*>) = (****)


------------------------------------------------------------------------------
selected :: PGOut p a => Select p a
selected = selecting def


------------------------------------------------------------------------------
selecting :: QueryRunner p a -> Select p a
selecting runner = Select (Procompose (Star pure) runner)


------------------------------------------------------------------------------
parse :: (a -> Either String b) -> Select p a -> Select p b
parse f (Select (Procompose (Star p) q)) =
    Select (Procompose (Star (p >=> f)) q)


------------------------------------------------------------------------------
select :: Traversable f => Run f -> Select p a -> Query p -> Transaction (f a)
select run (Select (Procompose (Star parser) runner)) q = do
    result <- traverse parser <$> run runner q
    case result of
        Left message -> throw (SelectParseError message)
        Right as -> pure as
