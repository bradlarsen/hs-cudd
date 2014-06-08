{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
             FlexibleInstances, MultiParamTypeClasses #-}
-- | Sentences in propositional logic.
module Prop
  ( Prop (..)
  , vars
  , maxVar
  , size
  , numVars
  ) where

import PropositionalPrelude
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

-- | A sentence in propositional logic with variable type 'v'.
data Prop v
  = PFalse
  | PTrue
  | PVar  !v
  | PNot  !(Prop v)
  | PAnd  !(Prop v) !(Prop v)
  | PNand !(Prop v) !(Prop v)
  | POr   !(Prop v) !(Prop v)
  | PNor  !(Prop v) !(Prop v)
  | PXor  !(Prop v) !(Prop v)
  | PXnor !(Prop v) !(Prop v)
  | PIte  !(Prop v) !(Prop v) !(Prop v)
  deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable)

vars :: (Ord v) => Prop v -> [v]
vars = Set.toList . Set.fromList . Foldable.toList

numVars :: (Ord v) => Prop v -> Int
numVars = Set.size . Set.fromList . Foldable.toList

maxVar :: (Ord v) => Prop v -> Maybe v
maxVar prop = case vars prop of
                [] -> Nothing
                vs -> Just (maximum vs)

size :: Prop v -> Int
size prop =
  case prop of
    PFalse         -> 1
    PTrue          -> 1
    PVar _v        -> 1
    PNot p1        -> 1 + size p1
    PAnd  p1 p2    -> 1 + size p1 + size p2
    POr   p1 p2    -> 1 + size p1 + size p2
    PXor  p1 p2    -> 1 + size p1 + size p2
    PNand p1 p2    -> 1 + size p1 + size p2
    PNor  p1 p2    -> 1 + size p1 + size p2
    PXnor p1 p2    -> 1 + size p1 + size p2
    PIte p1 p2 p3  -> 1 + size p1 + size p2 + size p3
