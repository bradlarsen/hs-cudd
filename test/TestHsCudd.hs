module Main where

import Cudd
import Prop

import Control.Monad (forM_, forM, liftM, foldM)
import Data.Maybe (fromJust, isJust)
import System.IO (stderr)
import Text.Printf (hPrintf, printf)

import Test.QuickCheck (Property, quickCheckWith, stdArgs,
                        Args (maxSuccess, maxDiscard), Testable,
                        verboseCheckWith, (==>), forAll, choose,
                        Positive (Positive), listOf)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.HUnit (Test (TestCase, TestList), assertBool)
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

prop_symbolicEvaluation :: Prop.Prop -> Property
prop_symbolicEvaluation prop = monadicIO $ do
  ok <- run $ do
          mgr     <- newMgr
          propBdd <- synthesizeBdd mgr prop
          andForM (assignments $ vars prop) $ \ass -> do
            let truthiness = eval ass prop
            bddTruthiness <- evalBdd ass mgr propBdd
            return (truthiness == bddTruthiness)
  assert ok
  run performGC

prop_projectionFunSize :: Property
prop_projectionFunSize = forAll (choose (0, 100000)) $ \idx -> monadicIO $ do
    ok <- run $ do
            mgr <- newMgr
            idxBdd <- bddIthVar mgr idx
            idxBddNumNodes <- bddNumNodes idxBdd
            size <- numNodes mgr
            return (idxBddNumNodes == 2 && size == 2)
    assert ok
    run performGC

mkCube :: Mgr -> [Bdd] -> IO Bdd
mkCube mgr vs = do
  true <- bddTrue mgr
  foldM bddAnd true vs

prop_existAbstract :: Prop.Prop -> Property
prop_existAbstract prop = let mv = maxVar prop in isJust mv ==>
  forAll (listOf $ choose (0, fromJust mv)) $ \vars -> monadicIO $ do
    ok <- run $ do
            mgr <- newMgr
            p   <- synthesizeBdd mgr prop
            vs  <- mapM (bddIthVar mgr) vars
            let exist p v = do pv  <- bddRestrict p v
                               pv' <- bddRestrict p =<< bddNot v
                               bddOr pv pv'
            ex' <- foldM exist p vs
            ex  <- bddExistAbstract p =<< mkCube mgr vs
            bddEqual ex ex'
    assert ok
    run performGC

prop_univAbstract :: Prop.Prop -> Property
prop_univAbstract prop = let mv = maxVar prop in isJust mv ==>
  forAll (listOf $ choose (0, fromJust mv)) $ \vars -> monadicIO $ do
    ok <- run $ do
            mgr <- newMgr
            p   <- synthesizeBdd mgr prop
            vs  <- mapM (bddIthVar mgr) vars
            let univ p v = do pv  <- bddRestrict p v
                              pv' <- bddRestrict p =<< bddNot v
                              bddAnd pv pv'
            ex' <- foldM univ p vs
            ex  <- bddUnivAbstract p =<< mkCube mgr vs
            bddEqual ex ex'
    assert ok
    run performGC

main :: IO ()
main = do
  putStrLn "### HUnit tests ###"
  _counts <- runTestTT unitTests

  putStrLn "### QuickCheck tests ###"
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => String -> p -> IO ()
      qc name prop = printf "%s: " name >> quickCheckWith conf prop
  qc "prop_symbolicEvaluation" prop_symbolicEvaluation
  qc "prop_projectionFunSize" prop_projectionFunSize
  qc "prop_existAbstract" prop_existAbstract
  qc "prop_univAbstract" prop_univAbstract
  return ()
