-- Sentences in propositional logic.
module Prop
  ( Prop (..)
  , vars
  , Assignment
  , assignments
  , eval
  , evalBdd
  , synthesizeBdd
  ) where

import qualified Cudd

import Control.Monad (liftM, liftM2, foldM)
import Data.IntSet (IntSet, empty, insert, union, member, toList, toAscList)
import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        frequency, choose, shrinkIntegral, vectorOf, sized)

data Prop
  = PFalse
  | PTrue
  | PVar Int    -- really, a non-negative number, indicating variable x_i
  | PNot Prop
  | PAnd Prop Prop
  | POr Prop Prop
  | PXor Prop Prop
  | PNand Prop Prop
  | PNor Prop Prop
  | PXnor Prop Prop
  deriving (Show, Eq)

vars' :: Prop -> IntSet
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

vars :: Prop -> [Int]
vars = toList . vars'

arbitraryProp :: [Int] -> Int -> Gen Prop
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

boundShrinkProp :: Int -> Prop -> [Prop]
boundShrinkProp bound prop
  | bound <= 0 = []
  | otherwise =
  case prop of
    PFalse      -> []
    PTrue       -> []
    PVar i      -> [ PVar i' | i' <- shrinkIntegral i, i' >= 0 ] ++
                   [ PFalse, PTrue ]
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
    bin :: (Prop -> Prop -> Prop) -> Prop -> Prop -> [Prop]
    bin op p1 p2 = [ op p1' p2' | p1' <- boundShrinkProp (pred bound) p1
                                , p2' <- boundShrinkProp (pred bound) p2 ] ++
                   [ p1, p2, PFalse, PTrue ]
instance Arbitrary Prop where
  arbitrary = do
    nVars <- choose (0, 10)
    maxVar <- choose (nVars, 100)
    indexes <- vectorOf nVars (choose (0, maxVar))
    sized $ arbitraryProp indexes
  shrink = boundShrinkProp 1

-- An assignment of truth values to variables.  x_i is true <=> i is in the set.
type Assignment = IntSet

-- | Constructs all possible assignments to the variables, which are denoted by
-- index.
assignments :: [Int] -> [Assignment]
assignments [] = [empty]
assignments (i : is) = concatMap assign (assignments is)
  where assign as = [ as, insert i as ]

eval :: Assignment -> Prop -> Bool
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

evalBdd :: Assignment -> Cudd.Bdd s -> Cudd.BddIO s Bool
evalBdd assigns bdd = do
  numVars  <- Cudd.numVars
  true     <- Cudd.bddTrue
  assigns' <- foldM (\b i -> do i' <- Cudd.bddIthVar i
                                if member i assigns
                                   then Cudd.bddAnd b i'
                                   else Cudd.bddAnd b =<< Cudd.bddNot i')
                     true [0..numVars - 1]
  res <- Cudd.bddRestrict bdd assigns'
  Cudd.bddToBool res

-- | Synthesize the sentence of propositional logic into a BDD.
synthesizeBdd :: Prop -> Cudd.BddIO s (Cudd.Bdd s)
synthesizeBdd prop =
  case prop of
    PFalse      -> Cudd.bddFalse
    PTrue       -> Cudd.bddTrue
    PVar i      -> Cudd.bddIthVar i
    PNot p      -> Cudd.bddNot =<< synthesizeBdd p
    PAnd p1 p2  -> bin Cudd.bddAnd p1 p2
    POr p1 p2   -> bin Cudd.bddOr p1 p2
    PXor p1 p2  -> bin Cudd.bddXor p1 p2
    PNand p1 p2 -> bin Cudd.bddNand p1 p2
    PNor p1 p2  -> bin Cudd.bddNor p1 p2
    PXnor p1 p2 -> bin Cudd.bddXnor p1 p2
  where bin f p1 p2 = do p1' <- synthesizeBdd p1
                         p2' <- synthesizeBdd p2
                         f p1' p2'
