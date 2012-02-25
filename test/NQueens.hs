-- N Queens using binary decision diagrams
--
-- The encoding uses a single Boolean variable for each of N^2 cells of the
-- board.
module Main (main) where

import Cudd
import Prop

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>), (<=<), forM_, forM, foldM, guard)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Text.Printf (printf, hPrintf)

modelNQueens :: Int -> Prop Int
modelNQueens n =
    conjoin ([ atLeastOneInRow i | i <- [0..n-1] ] ++
             [ safe (i, j) | i <- [0..n-1], j <- [0..n-1] ])
  where
    atLeastOneInRow i = conjoin [ disjoin [ var (i, j) | j <- [0..n-1] ] ]

    safe (i, j) =
      let impl p = nvar (i, j) `POr` p in
      let notInRow = conjoin [ impl $ nvar (i, j') | j' <- [0..n-1], j /= j' ] in
      let notInCol = conjoin [ impl $ nvar (i', j) | i' <- [0..n-1], i /= i' ] in
      let notInUpRight = conjoin [ impl $ nvar c | d <- [1..n-1]
                                                 , let c = (i - d, j + d)
                                                 , inBounds c ] in
      let notInDownRight = conjoin [ impl $ nvar c | d <- [1..n-1]
                                                   , let c = (i + d, j + d)
                                                   , inBounds c ] in
      conjoin [notInRow, notInCol, notInUpRight, notInDownRight]

    disjoin = foldl POr PFalse
    conjoin = foldl PAnd PTrue

    nvar = PNot . PVar . cellToIdx
    var = PVar . cellToIdx

    inBounds (i, j) = 0 <= i && i < n && 0 <= j && j < n
    cellToIdx (i, j) = n * i + j
    -- idxToCell idx = idx `divMod` n

printAssignment :: Int -> [(Int, VarAssign)] -> IO ()
printAssignment n assigns = do
  forM_ [0..n-1] $ \i -> do
    forM_ [0..n-1] $ \j -> do
      case lookup (n * i + j) assigns of
        Just Positive  -> putChar 'Q'
        Just Negative  -> putChar '.'
        Just DoNotCare -> putChar 'Q'
        Nothing        -> error "printAssignment: incomplete assignment"
    putChar '\n'

-- This sequence is OES A000170, the number of ways of placing n non-attacking
-- queens on an nxn board.
expectedNumSolutions :: [(Int, Double)]
expectedNumSolutions = [
    (1, 1)
  , (2, 0)
  , (3, 0)
  , (4, 2)
  , (5, 10)
  , (6, 4)
  , (7, 40)
  , (8, 92)
  , (9, 352)
  , (10, 724)
  , (11, 2680)
  , (12, 14200)
  , (13, 73712)
  , (14, 365596)
  , (15, 2279184)
  , (16, 14772512)
  , (17, 95815104)
  , (18, 666090624)
  , (19, 4968057848)
  , (20, 39029188884)
  , (21, 314666222712)
  , (22, 2691008701644)
  , (23, 24233937684440)
  , (24, 227514171973736)
  , (25, 2207893435808352)
  , (26, 22317699616364044)
  ]

main :: IO ()
main = forM_ [1..10] $ \n -> do
  printf "################\n"
  printf "n is %d\n" n
  let queensProp = modelNQueens n
  mgr <- newMgr
--  enableReorderingReporting mgr
--  enableDynamicReordering mgr cudd_reorder_window3
  queensBdd <- synthesizeBdd mgr queensProp
  printf "queensBdd uses %d nodes\n" =<< bddNumNodes queensBdd
  numSolutions <- bddCountMinterms queensBdd
  printf "%f solutions\n" numSolutions
  mOneSolution <- bddPickOneMinterm queensBdd
  case mOneSolution of
    Nothing -> return ()
    Just s  -> do printf "one solution:\n"
                  printAssignment n s
  case lookup n expectedNumSolutions of
    Just ns | numSolutions /= ns -> do printf "error: expected %f solutions\n" ns
                                       exitFailure
    _                            -> return ()
