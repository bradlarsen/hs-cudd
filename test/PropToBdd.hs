module PropToBdd
  ( evalBdd
  , synthesizeBdd) where

import qualified Cudd
import Prop
import HsCuddPrelude

evalBdd :: Assignment Int -> Cudd.Mgr -> Cudd.Bdd -> IO Bool
evalBdd assigns mgr bdd = do
  numVars  <- Cudd.numVars mgr
  true     <- Cudd.bddTrue mgr
  assigns' <- foldM (\b i -> do i' <- Cudd.bddIthVar mgr i
                                if isTrue i assigns
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
    bin f p1 p2 = join $ f <$> synthesizeBdd' p1 <*> synthesizeBdd' p2
