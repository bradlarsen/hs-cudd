-- | Strict lists with element type 'a'.
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric,
             FlexibleInstances, MultiParamTypeClasses #-}
module SList
  (
    SList(..)
  , slistFromList
  , sortSList
  ) where

import Control.Applicative ((<$>))
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.List (sort)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

import Test.SmallCheck.Series (Serial)
import Test.QuickCheck (Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  listOf)

data SList a
  = SNil
  | SCons !a !(SList a)
  deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (Serial m a) => Serial m (SList a)

slistFromList :: [a] -> SList a
slistFromList = foldr SCons SNil

sortSList :: (Ord a) => SList a -> SList a
sortSList = slistFromList . sort . Foldable.toList

instance (Arbitrary a) => Arbitrary (SList a) where
  arbitrary = slistFromList <$> listOf arbitrary
  shrink = map slistFromList . shrink . Foldable.toList

instance (CoArbitrary a) => CoArbitrary (SList a) where
  coarbitrary xs g = coarbitrary (Foldable.toList xs) g
