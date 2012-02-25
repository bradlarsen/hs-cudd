-- Some unit tests & property tests for the BDD library.
-- This could use some cleanup.  And more properties!
module Main where

import Cudd
import Prop
import PropGenerators
import PropToBdd

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, forM, liftM, foldM, when, join)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPrint)
import Text.Printf (hPrintf, printf)

import Test.QuickCheck
  (Property, quickCheckWithResult, stdArgs, Args (maxSuccess, maxDiscard),
   Testable, (==>), forAll, choose, Positive (Positive),
   listOf, Result (Failure, NoExpectedFailure), elements, Gen, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, assert, run, pre, pick)

import System.Mem (performGC)


prop_symbolicEvaluation :: Property
prop_symbolicEvaluation = monadicIO $ do
  prop <- pick arbitrary
  mgr  <- run newMgr
  bdd  <- run $ synthesizeBdd mgr prop
  forM_ (assignments $ vars prop) $ \ass -> do
     rhs <- run $ evalBdd ass mgr bdd
     assert (eval ass prop == rhs)
  run performGC

prop_symbolicEvaluationSame :: Property
prop_symbolicEvaluationSame = monadicIO $ do
  prop <- pick arbitrary
  eq   <- run $ do mgr  <- newMgr
                   bdd  <- synthesizeBdd mgr prop
                   bdd' <- synthesizeBdd mgr prop
                   bddEqual bdd bdd'
  assert eq
  run performGC

prop_projectionFunSize :: Property
prop_projectionFunSize = monadicIO $ do
  idx <- pick $ choose (0, 100000)
  eq  <- run $ do mgr <- newMgr
                  idxBdd         <- bddIthVar mgr idx
                  idxBddNumNodes <- bddNumNodes idxBdd
                  size           <- numNodes mgr
                  return (idxBddNumNodes == 2 && size == 2)
  assert eq
  run performGC

mkCube :: Mgr -> [Bdd] -> IO Bdd
mkCube mgr [] = bddTrue mgr
mkCube mgr (v : vs) = foldM bddAnd v vs

quantifier :: (Bdd -> Bdd -> IO Bdd) -> (Bdd -> Bdd -> IO Bdd) -> Property
quantifier q q' = monadicIO $ do
  maxVar <- pick $ choose (0, 10)
  vars   <- pick $ listOf $ choose (0, maxVar)
  prop   <- pick $ arbitraryPropWithVars vars
  eq     <- run $ do mgr     <- newMgr
                     propBdd <- synthesizeBdd mgr prop
                     varBdds <- mapM (bddIthVar mgr) vars
                     lhs     <- foldM q propBdd varBdds
                     rhs     <- q' propBdd =<< mkCube mgr varBdds
                     bddEqual lhs rhs
  assert eq
  run performGC

prop_existAbstract :: Property
prop_existAbstract = quantifier exist bddExistAbstract
  where exist p v = do pv  <- bddRestrict p v
                       pv' <- bddRestrict p =<< bddNot v
                       bddOr pv pv'

prop_univAbstract :: Property
prop_univAbstract = quantifier univ bddUnivAbstract
  where univ p v = do pv  <- bddRestrict p v
                      pv' <- bddRestrict p =<< bddNot v
                      bddAnd pv pv'

permutation :: (Eq a) => [a] -> Gen [a]
permutation =
  let loop perm [] = return perm
      loop perm vals = do
        val <- elements vals
        loop (val : perm) (delete val vals)
  in loop []

prop_reorderSymbolicEvaluation :: Prop.Prop Int -> Property
prop_reorderSymbolicEvaluation prop = monadicIO $ do
  let mv = maxVar prop
  pre (isJust mv && fromJust mv <= 10)
  mgr <- run newMgr
  propBdd <- run $ synthesizeBdd mgr prop
  perm <- pick (permutation [0..fromJust mv])
  run $ reorderVariables mgr perm
  forM_ (assignments $ vars prop) $ \ass -> do
    eq <- run $ (eval ass prop ==) <$> evalBdd ass mgr propBdd
    assert eq
  run performGC

-- This isn't named so well.
prop_reorderIdempotent :: Prop.Prop Int -> Property
prop_reorderIdempotent prop = monadicIO $ do
  let mv = maxVar prop
  pre (isJust mv && fromJust mv <= 10)
  mgr <- run newMgr
  propBdd <- run $ synthesizeBdd mgr prop
  perm <- pick (permutation [0..fromJust mv])
  run $ reorderVariables mgr perm
  propBdd' <- run $ synthesizeBdd mgr prop
  ok <- run (bddEqual propBdd propBdd')
  assert ok


main :: IO ()
main = do
  failed <- newIORef False
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => String -> p -> IO ()
      qc name prop = do printf "%s: " name
                        res <- quickCheckWithResult conf prop
                        case res of
                          Failure {}           -> writeIORef failed True
                          NoExpectedFailure {} -> writeIORef failed True
                          _                    -> return ()

  qc "prop_symbolicEvaluation" prop_symbolicEvaluation
  qc "prop_symbolicEvaluationSame" prop_symbolicEvaluationSame
  qc "prop_reorderSymbolicEvaluation" prop_reorderSymbolicEvaluation
  qc "prop_reorderIdempotent" prop_reorderIdempotent
  qc "prop_projectionFunSize" prop_projectionFunSize
  qc "prop_existAbstract" prop_existAbstract
  qc "prop_univAbstract" prop_univAbstract

  didFail <- readIORef failed
  if didFail then exitFailure else exitSuccess