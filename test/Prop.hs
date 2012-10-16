-- Sentences in propositional logic.
module Prop
  ( Prop (..)
  , vars
  , maxVar
  , Assignment
  , isTrue
  , assignments
  , eval
  ) where

import qualified Cudd

import HsCuddPrelude
import Data.Set (Set, empty, insert, union, member, toList, toAscList)

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
  deriving (Show, Eq)

vars' :: (Ord a) => Prop a -> Set a
vars' PFalse = empty
vars' PTrue = empty
vars' (PVar i) = insert i empty
vars' (PNot p) = vars' p
vars' (PAnd p1 p2) = union (vars' p1) (vars' p2)
vars' (POr p1 p2) = union (vars' p1) (vars' p2)
vars' (PXor p1 p2) = union (vars' p1) (vars' p2)
vars' (PNand p1 p2) = union (vars' p1) (vars' p2)
vars' (PNor p1 p2) = union (vars' p1) (vars' p2)
vars' (PXnor p1 p2) = union (vars' p1) (vars' p2)

vars :: (Ord a) => Prop a -> [a]
vars = toList . vars'

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
            PFalse      -> False
            PTrue       -> True
            PVar i      -> member i assigns
            PNot p      -> not (eval' p)
            PAnd p1 p2  -> eval' p1 && eval' p2
            POr p1 p2   -> eval' p1 || eval' p2
            PXor p1 p2  -> eval' p1 /= eval' p2
            PNand p1 p2 -> not (eval' p1) || not (eval' p2)
            PNor p1 p2  -> not (eval' p1) && not (eval' p2)
            PXnor p1 p2 -> eval' p1 == eval' p2
