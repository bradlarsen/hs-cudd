{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Sentences in propositional logic.
module Prop
  ( Prop (..)
  , vars
  , maxVar
  , Assignment
  , isTrue
  , assignments
  , eval
  ) where

import qualified Data.Set as Set
import Data.Set (Set, empty, insert, member)
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | A sentence in propositional logic with variable type 'a'.
data Prop a
  = PFalse
  | PTrue
  | PVar !a
  | PNot !(Prop a)
  | PAnd !(Prop a) !(Prop a)
  | POr !(Prop a) !(Prop a)
  | PXor !(Prop a) !(Prop a)
  | PNand !(Prop a) !(Prop a)
  | PNor !(Prop a) !(Prop a)
  | PXnor !(Prop a) !(Prop a)
  | PIte !(Prop a) !(Prop a) !(Prop a)
  deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable)

vars :: (Ord a) => Prop a -> [a]
vars = Set.toList . Set.fromList . Foldable.toList

maxVar :: (Ord a) => Prop a -> Maybe a
maxVar prop = case vars prop of
                [] -> Nothing
                vs -> Just (maximum vs)

-- An assignment of truth values to variables.  x_i is true <=> i is in the set.
type Assignment a = Set a

-- | Constructs all possible assignments to the variables
assignments :: Ord a => [a] -> [Assignment a]
assignments [] = [empty]
assignments (i : is) = concatMap assign (assignments is)
  where assign as = [ as, insert i as ]

isTrue :: Ord a => a -> Assignment a -> Bool
isTrue = member

eval :: Ord a => Assignment a -> Prop a -> Bool
eval assigns = eval'
  where eval' prop =
          case prop of
            PFalse        -> False
            PTrue         -> True
            PVar i        -> member i assigns
            PNot p        -> not (eval' p)
            PAnd p1 p2    -> eval' p1 && eval' p2
            POr p1 p2     -> eval' p1 || eval' p2
            PXor p1 p2    -> eval' p1 /= eval' p2
            PNand p1 p2   -> not (eval' p1) || not (eval' p2)
            PNor p1 p2    -> not (eval' p1) && not (eval' p2)
            PXnor p1 p2   -> eval' p1 == eval' p2
            PIte p1 p2 p3 -> eval' (if eval' p1 then p2 else p3)
