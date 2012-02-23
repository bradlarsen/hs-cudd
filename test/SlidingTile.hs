module Main (main) where

import Cudd
import Prop

import Control.Monad (guard)
import Data.List (sort)
import System.Mem (performGC)
import Text.Printf (printf)


isPermutationOf :: [Int] -> [Int] -> Bool
isPermutationOf vs vs' = sort vs == sort vs'

implies :: Prop -> Prop -> Prop
implies p q = PNot p `POr` q

conjoin = foldl PAnd PTrue
disjoin = foldl POr PFalse

-- Returns a proposition representing the nxn board with values specified
-- in row-major order according to perm.
modelBoard :: Int -> [Int] -> Prop
modelBoard n perm
 | not (isPermutationOf perm [0..n * n - 1]) =
     error "modelBoard: given board is not a permutation"
 | otherwise = cellAssigns
  where
    numVals = n * n
    vals = [0..n*n-1]
    cells = [(i, j) | i <- [0..n-1], j <- [0..n-1]]

    idxToCell 0 = (0, 0)
    idxToCell i = i `divMod` (numVals * i * n)

    cellHasVal (i, j) v = PVar (numVals * (i * n + j) + v)

    cellAssigns = conjoin $ do
      (i, v) <- zip [0..] perm
      v' <- vals
      let p = cellHasVal (idxToCell i) v'
      return $ if v == v' then p else PNot p

-- Returns a proposition representing all valid nxn boards.
modelBoards :: Int -> Prop
modelBoards n
  | n <= 0 = error "modelBoard: n is not positive"
  | otherwise =
      let numVals = n*n
          vals = [0..n*n-1]
          cells = [(i, j) | i <- [0..n-1], j <- [0..n-1]]
          -- One variable per possible value ([0..n*n-1]) per cell
          cellHasVal (i, j) v = PVar (numVals * (i * n + j) + v)
          cellAtLeastOne c = disjoin [ cellHasVal c v | v <- vals ]
          cellAtMostOne c =
            conjoin [ cellHasVal c v `implies` PNot (cellHasVal c v') |
                        v <- vals, v' <- vals, v /= v' ]
          cellsUnique = conjoin $ do
            c  <- cells
            c' <- cells
            guard (c /= c')
            v  <- vals
            return $ cellHasVal c v `implies` PNot (cellHasVal c' v)
       in conjoin ([cellAtLeastOne c `PAnd` cellAtMostOne c | c <- cells] ++
                   [cellsUnique])
      
            


main :: IO ()
main = do
  let startPerm = [9, 11, 10, 15, 12, 7, 8, 3, 13, 6, 14, 4, 5, 1, 0, 2]
  -- let goalPerm  = [0..15]
  mgr <- newMgr
  do oldNodeLimit <- nodeLimit mgr
     setNodeLimit mgr 20000000
     newNodeLimit <- nodeLimit mgr
     printf "changed node limit from %d to %d\n" oldNodeLimit newNodeLimit
  -- enableDynamicReordering mgr cudd_reorder_window3
  -- enableDynamicReordering mgr cudd_reorder_sift
  -- enableReorderingReporting mgr
  startBoard <- synthesizeBdd mgr (modelBoard 4 startPerm)
  -- goalBoard  <- modelBoard goalPerm
  performGC
  printf "%d BDD variables\n" =<< numVars mgr
  printf "%d BDD nodes\n" =<< numNodes mgr
  printf "startBoard has %.0f minterms\n" =<< bddCountMinterms startBoard
  printf "startBoard has %d BDD nodes\n" =<< bddNumNodes startBoard
  -- printf "goalBoard has %d minterms\n" =<< bddCountMinterms goalBoard
