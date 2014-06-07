-- | Simple evaluation for propositional sentences.

{-# LANGUAGE ScopedTypeVariables #-}
module PropEval
  ( Assignment
  , isTrue
  , assignments
  , eval
  ) where

import PropositionalPrelude hiding (empty)
import Data.Set (empty, insert, member)

import Prop

-- An assignment of truth values to variables.  x_i is true <=> i is in the set.
type Assignment a = Set a

-- | Constructs all possible assignments to the variables
assignments :: Ord v => [v] -> [Assignment v]
assignments [] = [empty]
assignments (i : is) = assignments is ++ map (insert i) (assignments is)

isTrue :: Ord v => v -> Assignment v -> Bool
isTrue = member

eval :: forall v. (Ord v) => Assignment v -> Prop v -> Bool
eval assigns = assigns `seq` eval'
  where
    eval' :: Prop v -> Bool
    eval' prop =
      case prop of
        PFalse        -> False
        PTrue         -> True
        PVar i        -> member i assigns
        PNot p        -> not (eval' p)
        PAnd p1 p2    -> eval' p1 && eval' p2
        PNand p1 p2   -> not (eval' p1 && eval' p2)
        POr p1 p2     -> eval' p1 || eval' p2
        PNor p1 p2    -> not (eval' p1 || eval' p2)
        PXor p1 p2    -> eval' p1 /= eval' p2
        PXnor p1 p2   -> eval' p1 == eval' p2
        PIte p1 p2 p3 -> eval' (if eval' p1 then p2 else p3)
{-# SPECIALIZE eval :: Assignment Int -> Prop Int -> Bool #-}
