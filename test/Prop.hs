{-# LANGUAGE FlexibleInstances #-}
-- Sentences in propositional logic.
module Prop
  ( Prop (..)
  , vars
  , maxVar
  , Assignment
  , assignments
  , eval
  , evalBdd
  , synthesizeBdd
  ) where

import qualified Cudd

import Control.Monad (liftM, liftM2, foldM)
import Data.Set (Set, empty, insert, union, member, toList, toAscList)
import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        frequency, choose, shrinkIntegral, vectorOf, sized)

data Prop a
  = PFalse
  | PTrue
  | PVar a
  | PNot (Prop a)
  | PAnd (Prop a) (Prop a)
  | POr (Prop a) (Prop a)
  | PXor (Prop a) (Prop a)
  | PNand (Prop a) (Prop a)
  | PNor (Prop a) (Prop a)
  | PXnor (Prop a) (Prop a)
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

arbitraryProp :: [a] -> Int -> Gen (Prop a)
arbitraryProp [] = const $ elements [PFalse, PTrue]
arbitraryProp vars = go
  where
    go maxNodes = if maxNodes <= 1 then term else nonterm
      where
        term = elements (PFalse : PTrue : map PVar vars)
        nonterm = oneof ([ liftM PNot $ go (maxNodes - 1) ] ++
                         [ bin c | c <- [PAnd, POr, PXor, PNand, PNor, PXnor] ])
        bin cons = do s1 <- choose (1, maxNodes - 1)
                      let s2 = maxNodes - s1
                      liftM2 cons (go s1) (go s2)

boundShrinkProp :: (Ord a, Arbitrary a) => Int -> Prop a -> [Prop a]
boundShrinkProp bound prop
  | bound <= 0 = []
  | otherwise =
  case prop of
    PFalse      -> []
    PTrue       -> []
    PVar i      -> [ PVar i' | i' <- shrink i ] ++ [ PFalse, PTrue ]
    PNot p      -> p : boundShrinkProp (pred bound) p ++
                   [ PNot p' | p' <- boundShrinkProp (pred bound) p ] ++
                   [ PFalse, PTrue ]
    PAnd p1 p2  -> bin PAnd p1 p2
    POr p1 p2   -> bin POr p1 p2
    PXor p1 p2  -> bin PXor p1 p2
    PNand p1 p2 -> bin PNand p1 p2
    PNor p1 p2  -> bin PNor p1 p2
    PXnor p1 p2 -> bin PXnor p1 p2
  where
    bin op p1 p2 = [ op p1' p2' | p1' <- boundShrinkProp (pred bound) p1
                                , p2' <- boundShrinkProp (pred bound) p2 ] ++
                   [ p1, p2, PFalse, PTrue ]

instance Arbitrary (Prop Int) where
  arbitrary = do
    nVars <- choose (0, 10)
    maxVar <- choose (nVars, 100)
    indexes <- vectorOf nVars (choose (0, maxVar))
    sized $ arbitraryProp indexes
  shrink = boundShrinkProp 1

-- An assignment of truth values to variables.  x_i is true <=> i is in the set.
type Assignment a = Set a

-- | Constructs all possible assignments to the variables
assignments :: Ord a => [a] -> [Assignment a]
assignments [] = [empty]
assignments (i : is) = concatMap assign (assignments is)
  where assign as = [ as, insert i as ]

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

evalBdd :: Assignment Int -> Cudd.Mgr -> Cudd.Bdd -> IO Bool
evalBdd assigns mgr bdd = do
  numVars  <- Cudd.numVars mgr
  true     <- Cudd.bddTrue mgr
  assigns' <- foldM (\b i -> do i' <- Cudd.bddIthVar mgr i
                                if member i assigns
                                   then Cudd.bddAnd b i'
                                   else Cudd.bddAnd b =<< Cudd.bddNot i')
                     true [0..numVars - 1]
  res <- Cudd.bddRestrict bdd assigns'
  Cudd.bddToBool res

-- | Synthesize the sentence of propositional logic into a BDD.
synthesizeBdd :: Cudd.Mgr -> Prop Int -> IO Cudd.Bdd
synthesizeBdd mgr = synthesizeBdd'
  where
    synthesizeBdd' prop =
      case prop of
        PFalse      -> Cudd.bddFalse mgr
        PTrue       -> Cudd.bddTrue mgr
        PVar i      -> Cudd.bddIthVar mgr i
        PNot p      -> Cudd.bddNot =<< synthesizeBdd' p
        PAnd p1 p2  -> bin Cudd.bddAnd p1 p2
        POr p1 p2   -> bin Cudd.bddOr p1 p2
        PXor p1 p2  -> bin Cudd.bddXor p1 p2
        PNand p1 p2 -> bin Cudd.bddNand p1 p2
        PNor p1 p2  -> bin Cudd.bddNor p1 p2
        PXnor p1 p2 -> bin Cudd.bddXnor p1 p2
    bin f p1 p2 = do p1' <- synthesizeBdd' p1
                     p2' <- synthesizeBdd' p2
                     f p1' p2'
