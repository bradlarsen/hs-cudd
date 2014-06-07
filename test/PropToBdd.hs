module PropToBdd
  ( evalBdd
  , synthesizeBdd
  ) where

import qualified Cudd
import Prop
import PropCSE
import PropEval
import HsCuddPrelude

import qualified Data.Map as Map
import Data.Map (Map, (!))

--import Debug.Trace (trace)
--import Text.Printf (printf)

evalBdd :: Assignment Int -> Cudd.Mgr -> Cudd.Bdd -> IO Bool
evalBdd assigns mgr bdd = do
  nVars  <- Cudd.numVars mgr
  true'  <- Cudd.bddTrue mgr
  assigns' <- foldM (\b i -> do i' <- Cudd.bddIthVar mgr i
                                if isTrue i assigns
                                   then Cudd.bddAnd b i'
                                   else Cudd.bddAnd b =<< Cudd.bddNot i')
                     true' [0..nVars - 1]
  res <- Cudd.bddRestrict bdd assigns'
  Cudd.bddToBool res

---- | Synthesize the sentence of propositional logic into a BDD.
--synthesizeBdd :: Cudd.Mgr -> Prop Int -> IO Cudd.Bdd
--synthesizeBdd mgr = synthesizeBdd'
--  where
--    synthesizeBdd' prop =
--      case prop of
--        PFalse        -> Cudd.bddFalse mgr
--        PTrue         -> Cudd.bddTrue mgr
--        PVar i        -> Cudd.bddIthVar mgr i
--        PNot p        -> Cudd.bddNot =<< synthesizeBdd' p
--        PAnd p1 p2    -> bin Cudd.bddAnd p1 p2
--        POr p1 p2     -> bin Cudd.bddOr p1 p2
--        PXor p1 p2    -> bin Cudd.bddXor p1 p2
--        PNand p1 p2   -> bin Cudd.bddNand p1 p2
--        PNor p1 p2    -> bin Cudd.bddNor p1 p2
--        PXnor p1 p2   -> bin Cudd.bddXnor p1 p2
--        PIte p1 p2 p3 -> bin Cudd.bddIte p1 p2 p3
--    bin f p1 p2 = join $ f <$> synthesizeBdd' p1 <*> synthesizeBdd' p2


type SynthesisMap = Map Idx Cudd.Bdd

-- | Synthesize the sentence of propositional logic into a BDD,
-- with a CSE pass first, and lowest-first scheduling.
synthesizeBdd :: Cudd.Mgr -> Prop Int -> IO Cudd.Bdd
synthesizeBdd mgr prop = do
    --trace (printf "prop: %s" (show prop)) $ do
    --trace (printf "schedule: %s" (show schedule)) $ do
    --trace (printf "root: %s" (show root)) $ do
    endMap <- foldM synthesize Map.empty (lowestFirstSchedule prop')
    return (endMap ! rootIndex prop')
  where
    prop' :: CSEProp Int
    prop' = cse prop

    synthesize :: SynthesisMap -> (Idx, Prop' Int) -> IO SynthesisMap
    synthesize done (i, p) =
      --trace (printf "synthesizing %s" (show (i, p))) $
       (\v -> Map.insert i v done) <$> case p of
         PHCFalse        -> Cudd.bddFalse mgr
         PHCTrue         -> Cudd.bddTrue mgr
         PHCVar v        -> Cudd.bddIthVar mgr v
         PHCNot i1       -> Cudd.bddNot  (done!i1)
         PHCAnd  i1 i2   -> Cudd.bddAnd (done!i1) (done!i2)
         PHCNand i1 i2   -> Cudd.bddNot =<< Cudd.bddAnd (done!i1) (done!i2)
         PHCOr   i1 i2   -> Cudd.bddOr (done!i1) (done!i2)
         PHCNor  i1 i2   -> Cudd.bddNot =<< Cudd.bddOr (done!i1) (done!i2)
         PHCXor  i1 i2   -> Cudd.bddXor  (done!i1) (done!i2)
         PHCXnor i1 i2   -> Cudd.bddXnor (done!i1) (done!i2)
         PHCIte i1 i2 i3 -> Cudd.bddIte  (done!i1) (done!i2) (done!i3)
