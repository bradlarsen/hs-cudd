module Main where

import Cudd

import Control.Monad.IO.Class (liftIO)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Test.HUnit
import Test.HUnit.Text

sinkTests :: Test
sinkTests = TestList [
    TestCase $ runBddIO $ do
      false <- bddFalse
      faf <- bddAnd false false
      liftIO $ assertEqual "false /\\ false == false" faf false
  , TestCase $ runBddIO $ do
      true <- bddTrue
      tat <- bddAnd true true
      liftIO $ assertEqual "true /\\ true == true" tat true
  , TestCase $ runBddIO $ do
      true <- bddTrue
      false <- bddFalse
      taf <- bddAnd true false
      liftIO $ assertEqual "true /\\ false == false" taf false
      fat <- bddAnd false true
      liftIO $ assertEqual "false /\\ true == false" fat false
  ]

main :: IO ()
main = do
  _counts <- runTestTT sinkTests
  return ()
