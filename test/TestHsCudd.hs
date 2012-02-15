module Main where

import Cudd
import Prop

import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (liftIO)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Test.QuickCheck (Property, quickCheckWith, stdArgs,
                        Args (maxSuccess, maxDiscard), Testable,
                        verboseCheckWith, (==>), forAll, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.HUnit (Test (TestCase, TestList), assertEqual, assertBool)
import Test.HUnit.Text (runTestTT)

import System.Mem (performGC)


unitTests :: Test
unitTests = TestList [
    TestCase $ runBddIO $ do
      true    <- bddTrue
      false   <- bddFalse
      true'   <- bddNot true
      false'  <- bddNot false
      true''  <- bddNot true'
      false'' <- bddNot false'
      liftIO $ assertBool "true /= false" (true /= false)
      liftIO $ assertEqual "~ true == false" true' false
      liftIO $ assertEqual "~ false == true" false' true
      liftIO $ assertEqual "~ ~ false == false" false'' false
      liftIO $ assertEqual "~ ~ true == true" true'' true
  ]


andForM :: (Monad m) => [a] -> (a -> m Bool) -> m Bool
andForM [] _f = return True
andForM (v : vs) f = do
  v' <- f v
  if v' then andForM vs f
        else return False

prop_symbolicEvaluation :: Prop.Prop -> Property
prop_symbolicEvaluation prop = monadicIO $ do
  ok <- run $ runBddIO $ do
          propBdd <- synthesizeBdd prop
          andForM (assignments $ vars prop) $ \ass -> do
            let truthiness = eval ass prop
            bddTruthiness <- evalBdd ass propBdd
            return (truthiness == bddTruthiness)
  assert ok

prop_projectionFunSize :: Property
prop_projectionFunSize = forAll (choose (0, 10000)) $ \idx -> monadicIO $ do
    ok <- run $ runBddIO $ do
            idxBdd <- bddIthVar idx
            idxBddSize <- bddSize idxBdd
            size <- numNodes
            return (idxBddSize == 2 && size == 2)
    assert ok

prop_projectionDoesNotChangeNodes :: Prop.Prop -> Property
prop_projectionDoesNotChangeNodes prop = monadicIO $ do
  ok <- run $ runBddIO $ do
          propBdd <- synthesizeBdd prop
          liftIO performGC
          propBddSize <- bddSize propBdd
          size <- numNodes
          ok <- andForM (vars prop) $ \i -> do
                  iBdd <- bddIthVar i
                  size' <- numNodes
                  propBddSize' <- bddSize propBdd
                  return (size == size' && propBddSize == propBddSize')
          return ok
  assert ok


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
  qc prop_projectionDoesNotChangeNodes
  return ()
