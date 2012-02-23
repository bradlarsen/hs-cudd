-- This program generates two graphs of BDDs representing the function
--
--     (x1 <=> y1) && (x2 <=> y2)
--
-- in GraphViz dot format.  This demonstrates the difference in number of nodes
-- between two different variable orders.
module Main where

import Cudd
import Prop
import System.IO (Handle, stdout, stderr, withFile, IOMode (WriteMode))
import Text.Printf (hPrintf)

main :: IO ()
main = do
  mgr <- newMgr
  let f = PAnd (PVar 0 `PXnor` PVar 1) (PVar 2 `PXnor` PVar 3)
  fBdd <- synthesizeBdd mgr f
  let fName = "(x1 <=> y1) && (x2 <=> y2)"
  let inputNames = ["x1", "y1", "x2", "y2"]
  withFile "cudd-out-1.dot" WriteMode $ \out -> do
    bddToDot [(fBdd, fName)] inputNames out
  reorderVariables mgr [0, 2, 1, 3]
  withFile "cudd-out-2.dot" WriteMode $ \out -> do
    bddToDot [(fBdd, fName)] inputNames out
