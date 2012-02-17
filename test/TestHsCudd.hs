module Main where

import Cudd
import Prop

import Control.Monad (forM_, forM, liftM)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Test.QuickCheck (Property, quickCheckWith, stdArgs,
                        Args (maxSuccess, maxDiscard), Testable,
                        verboseCheckWith, (==>), forAll, choose)
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
prop_projectionFunSize = forAll (choose (0, 10000)) $ \idx -> monadicIO $ do
    ok <- run $ do
            mgr <- newMgr
            idxBdd <- bddIthVar mgr idx
            idxBddSize <- bddSize idxBdd
            size <- numNodes mgr
            return (idxBddSize == 2 && size == 2)
    assert ok
    run performGC


main :: IO ()
main = do
  putStrLn "### HUnit tests ###"
  _counts <- runTestTT unitTests

  putStrLn "### QuickCheck tests ###"
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => p -> IO ()
      qc = quickCheckWith conf
      -- qc = verboseCheckWith conf
  qc prop_symbolicEvaluation
  qc prop_projectionFunSize
  return ()
