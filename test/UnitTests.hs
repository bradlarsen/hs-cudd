module Main (main) where

import Cudd
import HsCuddPrelude

import Test.HUnit (Test (TestCase, TestList), assertBool, errors, failures)
import Test.HUnit.Text (runTestTT)

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
      assertBool "true /= false" =<< not <$> bddEqual true false
      assertBool "~ true == false" =<< bddEqual true' false
      assertBool "~ false == true" =<< bddEqual false' true
      assertBool "~ ~ false == false" =<< bddEqual false'' false
      assertBool "~ ~ true == true" =<< bddEqual true'' true
  , TestCase $ do
      mgr    <- newMgr
      true   <- bddTrue mgr
      false  <- bddFalse mgr
      result <- bddRestrict true false
      assertBool "result /= true" =<< not <$> bddEqual true result
      assertBool "result == false" =<< bddEqual false result
  ]

main :: IO ()
main = do
  counts <- runTestTT unitTests
  if errors counts /= 0 || failures counts /= 0
     then exitFailure
     else exitSuccess
