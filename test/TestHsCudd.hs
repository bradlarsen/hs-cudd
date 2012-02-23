-- Some unit tests & property tests for the BDD library.
-- This could use some cleanup.  And more properties!
module Main where

import Cudd
import Prop

import Control.Monad (forM_, forM, liftM, foldM, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPrint)
import Text.Printf (hPrintf, printf)

import Test.QuickCheck
  (Property, quickCheckWithResult, stdArgs, Args (maxSuccess, maxDiscard),
   Testable, (==>), forAll, choose, Positive (Positive),
   listOf, Result (Failure, NoExpectedFailure), elements, Gen)
import Test.QuickCheck.Monadic (monadicIO, assert, run, pre, pick)

import Test.HUnit (Test (TestCase, TestList), assertBool, errors, failures)
import Test.HUnit.Text (runTestTT)

import System.Mem (performGC)


unitTests :: Test
unitTests = TestList [
    TestCase $ do
      mgr     <- newMgr
      true    <- bddTrue mgr
      false   <- bddFalse mgr
      true'   <- bddNot true
      false'  <- bddNot false
      true''  <- bddNot true'
      false'' <- bddNot false'
      assertBool "true /= false" =<< liftM not (bddEqual true false)
      assertBool "~ true == false" =<< bddEqual true' false
      assertBool "~ false == true" =<< bddEqual false' true
      assertBool "~ ~ false == false" =<< bddEqual false'' false
      assertBool "~ ~ true == true" =<< bddEqual true'' true
  , TestCase $ do
      mgr    <- newMgr
      true   <- bddTrue mgr
      false  <- bddFalse mgr
      result <- bddRestrict true false
      assertBool "result /= true" =<< liftM not (bddEqual true result)
      assertBool "result == false" =<< bddEqual false result
  ]


andForM :: (Monad m) => [a] -> (a -> m Bool) -> m Bool
andForM [] _f = return True
andForM (v : vs) f = do
  v' <- f v
  if v' then andForM vs f
        else return False

bddProp :: (Mgr -> IO Bool) -> Property
bddProp act = monadicIO $ do
  ok <- run (newMgr >>= act)
  run performGC
  assert ok

prop_symbolicEvaluation :: Prop.Prop -> Property
prop_symbolicEvaluation prop = bddProp $ \mgr -> do
  propBdd <- synthesizeBdd mgr prop
  andForM (assignments $ vars prop) $ \ass -> do
    let truthiness = eval ass prop
    bddTruthiness <- evalBdd ass mgr propBdd
    return (truthiness == bddTruthiness)

prop_projectionFunSize :: Property
prop_projectionFunSize =
  forAll (choose (0, 100000)) $ \idx -> bddProp $ \mgr -> do
    idxBdd <- bddIthVar mgr idx
    idxBddNumNodes <- bddNumNodes idxBdd
    size <- numNodes mgr
    return (idxBddNumNodes == 2 && size == 2)

mkCube :: Mgr -> [Bdd] -> IO Bdd
mkCube mgr vs = do
  true <- bddTrue mgr
  foldM bddAnd true vs

prop_existAbstract :: Prop.Prop -> Property
prop_existAbstract prop = let mv = maxVar prop in isJust mv ==>
  forAll (listOf $ choose (0, fromJust mv)) $ \vars -> bddProp $ \mgr -> do
    p   <- synthesizeBdd mgr prop
    vs  <- mapM (bddIthVar mgr) vars
    let exist p v = do pv  <- bddRestrict p v
                       pv' <- bddRestrict p =<< bddNot v
                       bddOr pv pv'
    ex' <- foldM exist p vs
    ex  <- bddExistAbstract p =<< mkCube mgr vs
    bddEqual ex ex'

prop_univAbstract :: Prop.Prop -> Property
prop_univAbstract prop = let mv = maxVar prop in isJust mv ==>
  forAll (listOf $ choose (0, fromJust mv)) $ \vars -> bddProp $ \mgr -> do
    p   <- synthesizeBdd mgr prop
    vs  <- mapM (bddIthVar mgr) vars
    let univ p v = do pv  <- bddRestrict p v
                      pv' <- bddRestrict p =<< bddNot v
                      bddAnd pv pv'
    ex' <- foldM univ p vs
    ex  <- bddUnivAbstract p =<< mkCube mgr vs
    bddEqual ex ex'

permutation :: (Eq a) => [a] -> Gen [a]
permutation =
  let loop perm [] = return perm
      loop perm vals = do
        val <- elements vals
        loop (val : perm) (delete val vals)
  in loop []

prop_reorderSymbolicEvaluation :: Prop.Prop -> Property
prop_reorderSymbolicEvaluation prop = monadicIO $ do
  let mv = maxVar prop
  pre (isJust mv && fromJust mv <= 10)
  mgr <- run newMgr
  propBdd <- run $ synthesizeBdd mgr prop
  perm <- pick (permutation [0..fromJust mv])
  run $ reorderVariables mgr perm
  ok <- run $ andForM (assignments $ vars prop) $ \ass -> do
          let truthiness = eval ass prop
          bddTruthiness <- evalBdd ass mgr propBdd
          return (truthiness == bddTruthiness)
  assert ok

-- This isn't named so well.
prop_reorderIdempotent :: Prop.Prop -> Property
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

  putStrLn "### HUnit tests ###"
  counts <- runTestTT unitTests
  when (errors counts /= 0 || failures counts /= 0) $ do
    writeIORef failed True

  putStrLn "### QuickCheck tests ###"
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => String -> p -> IO ()
      qc name prop = do printf "%s: " name
                        res <- quickCheckWithResult conf prop
                        case res of
                          Failure {}           -> writeIORef failed True
                          NoExpectedFailure {} -> writeIORef failed True
                          _                    -> return ()

  qc "prop_symbolicEvaluation" prop_symbolicEvaluation
  qc "prop_reorderSymbolicEvaluation" prop_reorderSymbolicEvaluation
  qc "prop_reorderIdempotent" prop_reorderIdempotent
  qc "prop_projectionFunSize" prop_projectionFunSize
  qc "prop_existAbstract" prop_existAbstract
  qc "prop_univAbstract" prop_univAbstract

  didFail <- readIORef failed
  if didFail then exitFailure else exitSuccess
