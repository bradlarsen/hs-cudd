module Main where

import Cudd
import Prop

import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (liftIO)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Test.QuickCheck (Property, quickCheckWith, stdArgs,
                        Args (maxSuccess, maxDiscard), Testable,
                        verboseCheckWith, (==>))
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.HUnit (Test (TestCase, TestList), assertEqual, assertBool)
import Test.HUnit.Text (runTestTT)

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

symbolicEvaluation :: Prop.Prop -> Property
symbolicEvaluation prop = monadicIO $ do
  eq <- run $ runBddIO $ do
          propBdd <- synthesizeBdd prop
          forM (assignments $ vars prop) $ \ass -> do
            let truthiness = eval ass prop
            bddTruthiness <- evalBdd ass propBdd
            return (truthiness == bddTruthiness)
  assert (and eq)

prop_projectionDoesNotAddNodes :: Prop.Prop -> Property
prop_projectionDoesNotAddNodes prop = monadicIO $ do
  eq <- run $ runBddIO $ do
          propBdd <- synthesizeBdd prop
          size <- numNodes
          forM (vars prop) $ \i -> do
            iBdd <- bddIthVar i
            size' <- numNodes
            return (size == size')
  assert (and eq)


main :: IO ()
main = do
  putStrLn "### HUnit tests ###"
  _counts <- runTestTT unitTests

  putStrLn "### QuickCheck tests ###"
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => p -> IO ()
      qc = quickCheckWith conf
  qc symbolicEvaluation
  qc prop_projectionDoesNotAddNodes
  return ()
