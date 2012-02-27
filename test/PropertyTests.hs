-- Some unit tests & property tests for the BDD library.
-- This could use some cleanup.  And more properties!
module Main where

import Cudd hiding (VarAssign (..))
import Prop
import PropGenerators
import PropToBdd

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, forM, liftM, foldM, when, join, guard)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (lookup, delete, splitAt)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdout, stderr, hPrint, hFlush)
import Text.Printf (hPrintf, printf)

import Test.QuickCheck
  (Property, quickCheckWithResult, verboseCheckWithResult,
   Args (maxSuccess, maxDiscard), stdArgs,
   Testable, (==>), forAll, choose,
   listOf, Result (Failure, NoExpectedFailure), elements, Gen,
   Arbitrary (arbitrary, shrink),
   NonNegative (NonNegative))
import Test.QuickCheck.Monadic (monadicIO, assert, run, pre, pick)

import System.Mem (performGC)

import System.IO (withFile, IOMode (WriteMode))


prop_symbolicEvaluation :: Prop Int -> Property
prop_symbolicEvaluation prop = monadicIO $ do
  mgr  <- run newMgr
  bdd  <- run $ synthesizeBdd mgr prop
  forM_ (assignments $ vars prop) $ \ass -> do
     rhs <- run $ evalBdd ass mgr bdd
     assert (eval ass prop == rhs)
  run performGC

prop_symbolicEvaluationSame :: Prop Int -> Property
prop_symbolicEvaluationSame prop = monadicIO $ do
  eq   <- run $ do mgr  <- newMgr
                   bdd  <- synthesizeBdd mgr prop
                   bdd' <- synthesizeBdd mgr prop
                   bddEqual bdd bdd'
  assert eq
  run performGC

prop_projectionFunSize :: NonNegative Int -> Property
prop_projectionFunSize (NonNegative idx) =
  idx <= 100000 ==>
  monadicIO $ do
    eq  <- run $ do mgr <- newMgr
                    idxBdd         <- bddIthVar mgr idx
                    idxBddNumNodes <- bddNumNodes idxBdd
                    size           <- numNodes mgr
                    return (idxBddNumNodes == 2 && size == 2)
    assert eq
    run performGC


mkCube :: Mgr -> [Bdd] -> IO Bdd
mkCube mgr [] = bddTrue mgr
mkCube mgr (v : vs) = foldM bddAnd v vs

quantifier :: (Bdd -> Bdd -> IO Bdd) -> (Bdd -> Bdd -> IO Bdd) -> Prop Int -> Property
quantifier q q' prop = monadicIO $ do
  eq     <- run $ do mgr     <- newMgr
                     propBdd <- synthesizeBdd mgr prop
                     varBdds <- mapM (bddIthVar mgr) (vars prop)
                     lhs     <- foldM q propBdd varBdds
                     rhs     <- q' propBdd =<< mkCube mgr varBdds
                     bddEqual lhs rhs
  assert eq
  run performGC

prop_existAbstract :: Prop Int -> Property
prop_existAbstract = quantifier exist bddExistAbstract
  where exist p v = do pv  <- bddRestrict p v
                       pv' <- bddRestrict p =<< bddNot v
                       bddOr pv pv'

prop_univAbstract :: Prop Int -> Property
prop_univAbstract = quantifier univ bddUnivAbstract
  where univ p v = do pv  <- bddRestrict p v
                      pv' <- bddRestrict p =<< bddNot v
                      bddAnd pv pv'

permutation :: (Eq a) => [a] -> Gen [a]
permutation =
  let loop perm [] = return perm
      loop perm vals = do
        val <- elements vals
        loop (val : perm) (delete val vals)
  in loop []

prop_reorderSymbolicEvaluation :: Prop Int -> Property
prop_reorderSymbolicEvaluation prop = monadicIO $ do
  let mv = maxVar prop
  pre (isJust mv && fromJust mv <= 10)
  mgr <- run newMgr
  propBdd <- run $ synthesizeBdd mgr prop
  perm <- pick (permutation [0..fromJust mv])
  run $ reorderVariables mgr perm
  forM_ (assignments $ vars prop) $ \ass -> do
    eq <- run $ (eval ass prop ==) <$> evalBdd ass mgr propBdd
    assert eq
  run performGC

-- This isn't named so well.
prop_reorderIdempotent :: Prop Int -> Property
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


-- Does the proposition have at least 2 variables?
nontrivial :: Prop Int -> Bool
nontrivial prop = case maxVar prop of
                    Nothing -> False
                    Just i  -> 1 <= i

unsafeMaxVar :: Prop Int -> Int
unsafeMaxVar = fromJust . maxVar

data SplitProp = SplitProp { prop   :: Prop Int
                           , i1     :: Int
                           , prefix :: [Int]
                           , suffix :: [Int]
                           }
  deriving (Eq, Show)

mkSplitProp :: String -> Prop Int -> Int -> [Int] -> [Int] -> SplitProp
mkSplitProp func prop i1 prefix suffix =
  if not (nontrivial prop)
    then err "trivial prop"
  else let mv = fromJust (maxVar prop) in
       if not (i1 < mv)
         then err "bad i1"
       else if any (\pv -> pv > mv) prefix
         then err "bad prefix values"
       else if any (\sv -> sv > mv) suffix
         then err "bad suffix values"
       else if any (\pv -> any (\sv -> sv <= pv) suffix) prefix
         then err "bad prefix/suffix"
       else SplitProp prop i1 prefix suffix
  where err msg = error (func ++ ": " ++ msg)

i2 :: SplitProp -> Int
i2 = succ . i1

splitPropMaxVar :: SplitProp -> Int
splitPropMaxVar sp = fromJust (maxVar (prop sp))

split2 :: [a] -> Gen ([a], [a])
split2 vs = do
  let len = length vs
  i <- choose (0, len)
  return (splitAt i vs)

split3 :: [a] -> Gen ([a], [a], [a])
split3 vs = do
  let len = length vs
  j1 <- choose (0, len)
  j2 <- choose (0, len)
  let (i1, i2) = (min j1 j2, max j1 j2)
  return (take i1 vs, drop i1 (take i2 vs), drop i2 vs)

genSplitProp :: Prop Int -> Gen SplitProp
genSplitProp prop
  | nontrivial prop = do
      i1 <- choose (0, unsafeMaxVar prop - 1)
      let i2 = succ i1
      (prefix, suffix) <- split2 ([0..i1 - 1] ++ [i2 + 1..unsafeMaxVar prop])
      return $ mkSplitProp "genSplitProp" prop i1 prefix suffix
  | otherwise = error "trivial prop"

shrinkPrefixSuffix :: SplitProp -> [SplitProp]
shrinkPrefixSuffix sp = do
  let vs = prefix sp ++ suffix sp
  (prefix', suffix') <- [ splitAt i vs | i <- [0..length (prefix sp) - 2] ]
  return $ mkSplitProp "shrinkPrefixSuffix" (prop sp) (i1 sp) prefix' suffix'

shrinkIndexes :: SplitProp -> [SplitProp]
shrinkIndexes sp = do
  i1' <- shrink (i1 sp)
  let i2' = succ i1'
  let (prefix', suffix') = ([0..i1' - 1], [i2' + 1..unsafeMaxVar (prop sp)])
  return $ mkSplitProp "shrinkIndexes" (prop sp) i1' prefix' suffix'

shrinkProp :: SplitProp -> [SplitProp]
shrinkProp sp = do
  prop' <- shrink (prop sp)
  guard (nontrivial prop')
  let mv = unsafeMaxVar prop'
  guard (i2 sp <= mv)
  let prefix' = takeWhile (<= mv) (prefix sp)
  let suffix' = takeWhile (<= mv) (suffix sp)
  return $ mkSplitProp "shrinkProp" prop' (i1 sp) prefix' suffix'

instance Arbitrary SplitProp where
  arbitrary = let loop = do p <- arbitrary
                            if nontrivial p then return p else loop
               in loop >>= genSplitProp
  shrink sp = shrinkProp sp ++ shrinkIndexes sp ++ shrinkPrefixSuffix sp





prop_varOrderInvariant :: SplitProp -> Property
prop_varOrderInvariant sp =
  let propMaxVar = unsafeMaxVar (prop sp) in
  monadicIO $ do
    (mgr, propBdd, i1i2Size, i2i1Size) <- run $ do
        mgr <- newMgr
        propBdd <- synthesizeBdd mgr (prop sp)
        i1i2Size <- bddNumNodes propBdd
        let i2i1Order = [0..i1 sp - 1] ++ [i2 sp, i1 sp] ++ [i2 sp + 1..propMaxVar]
        reorderVariables mgr i2i1Order
        i2i1Size <- bddNumNodes propBdd
        return (mgr, propBdd, i1i2Size, i2i1Size)
    pre (i1i2Size <= i2i1Size)

    let order1 = prefix sp ++ [i1 sp, i2 sp] ++ suffix sp
    let order2 = prefix sp ++ [i2 sp, i1 sp] ++ suffix sp

    run $ reorderVariables mgr order1
    order1Size <- run $ bddNumNodes propBdd
    run $ reorderVariables mgr order2
    order2Size <- run $ bddNumNodes propBdd
    when (order1Size > order2Size) $ run $ do
      withFile "order1.dot" WriteMode $ \out -> do
        reorderVariables mgr order1
        bddToDot [(propBdd, show $ prop sp)] (map show [0..propMaxVar]) out
      withFile "order2.dot" WriteMode $ \out -> do
        reorderVariables mgr order2
        bddToDot [(propBdd, show $ prop sp)] (map show [0..propMaxVar]) out
    assert (order1Size <= order2Size)




main :: IO ()
main = do
  failed <- newIORef False
  let conf = stdArgs { maxSuccess = 100, maxDiscard = 1000 }
  let qc :: Testable p => String -> p -> IO ()
      qc name prop = do printf "%s: " name >> hFlush stdout
                        --res <- verboseCheckWithResult conf prop
                        res <- quickCheckWithResult conf prop
                        case res of
                          Failure {}           -> writeIORef failed True
                          NoExpectedFailure {} -> writeIORef failed True
                          _                    -> return ()

  qc "prop_symbolicEvaluation" prop_symbolicEvaluation
  qc "prop_symbolicEvaluationSame" prop_symbolicEvaluationSame
  qc "prop_reorderSymbolicEvaluation" prop_reorderSymbolicEvaluation
  qc "prop_reorderIdempotent" prop_reorderIdempotent
  qc "prop_projectionFunSize" prop_projectionFunSize
  qc "prop_existAbstract" prop_existAbstract
  qc "prop_univAbstract" prop_univAbstract
  qc "prop_varOrderInvariant" prop_varOrderInvariant

  didFail <- readIORef failed
  if didFail then exitFailure else exitSuccess