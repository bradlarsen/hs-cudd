-- This module provides a higher-level interface to Cudd.Raw.  This
-- higher-level interface exposes two types:
-- 
--     * [BddIO s a], the type of BDD computations with state thread [s] and
--       return type [a]
--     * [Bdd s], the type of BDDs with state thread [s]
-- 
-- The only way to run a computation of type [BddIO s a] is to use the
-- rank-2 polymorphic function [runBddIO :: (forall s. BddIO s a) -> IO a]
-- The type of [runBddIO] ensures that no references to BDDs escape, which in
-- turn allows prompt, safe deallocation of the resources used by a BDD
-- computation.
--
-- This code will surely only work right with GHC >= 6.10.2, which
-- apparently guarantees that finalizers will be run promptly.  Each
-- [BddIO s a] computation, when run, creates a new CUDD manager, which
-- must not be freed until all its BDDs have been returned to it.  Each
-- [Bdd s] value is assocated with a finalizer that calls
-- [Cudd_RecursiveDeref].  To ensure that all finalizer for [Bdd s]
-- values have been finalized before freeing the CUDD manager of the
-- computation, [System.Mem.performGC] is called before calling
-- [Cudd_Quit].  This is dodgy but seems to do the right thing in
-- recent versions of GHC.
{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}
module Cudd
  ( BddIO
  , runBddIO
  , CuddException (..)
  , Bdd
  , bddTrue
  , bddFalse
  , bddNewVar
  , bddIthVar
  , bddNot
  , bddAnd
  , bddOr
  , bddXor
  , bddNand
  , bddNor
  , bddXnor
  , bddRestrict
  , numVars
  , numNodes
  , bddSize
  , bddToBool
  ) where

import Cudd.Raw

import Foreign (nullPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr, newForeignPtrEnv)

import Control.Exception (Exception, finally, throw)
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)
import System.Mem (performGC)

newtype Bdd s = Bdd { unBdd :: ForeignPtr DdNodeT }
  deriving (Eq, Show)

newtype BddIO s a = BddIO { unBddIO :: DdManager -> IO a }

instance Monad (BddIO s) where
  return a = BddIO $ \_mgr -> return a
  m >>= f = BddIO $ \mgr -> do
              v <- unBddIO m mgr
              unBddIO (f v) mgr

instance MonadIO (BddIO s) where
  liftIO a = BddIO $ \_mgr -> a


data CuddException
  = CuddNoError
  | CuddMemoryOut
  | CuddTooManyNodes
  | CuddMaxMemExceeded
  | CuddTimeoutExpired
  | CuddInvalidArg
  | CuddInternalError
  deriving (Eq, Ord, Show, Typeable)

instance Exception CuddException

toCuddException :: Cudd_ErrorType -> CuddException
toCuddException err
  | err == cudd_no_error         = CuddNoError
  | err == cudd_memory_out       = CuddMemoryOut
  | err == cudd_too_many_nodes   = CuddTooManyNodes
  | err == cudd_max_mem_exceeded = CuddMaxMemExceeded
  | err == cudd_timeout_expired  = CuddTimeoutExpired
  | err == cudd_internal_error   = CuddInvalidArg
  | err == cudd_internal_error   = CuddInternalError
  | otherwise = error "Cudd.Raw.toCuddException: bad Cudd_ErrorType!"


mkBdd :: DdManager -> DdNode -> IO (Bdd s)
mkBdd mgr dd = do
  when (dd == nullPtr) $ do
    err <- cudd_ReadErrorCode mgr
    throw (toCuddException err)
  cudd_Ref dd
  dd' <- newForeignPtrEnv cudd_RecursiveDeref_p mgr dd
  return $ Bdd dd'



newDefaultManager :: IO DdManager
newDefaultManager = cudd_Init 0 0 cudd_unique_slots cudd_cache_slots 0

runBddIO :: (forall s. BddIO s a) -> IO a
runBddIO f = do
  mgr <- newDefaultManager
  unBddIO f mgr `finally` (performGC >> cudd_Quit mgr)
  

bddTrue :: BddIO s (Bdd s)
bddTrue = BddIO $ \mgr ->
  cudd_ReadOne mgr >>= mkBdd mgr

bddFalse :: BddIO s (Bdd s)
bddFalse = BddIO $ \mgr ->
  cudd_ReadLogicZero mgr >>= mkBdd mgr

bddNewVar :: BddIO s (Bdd s)
bddNewVar = BddIO $ \mgr ->
  cudd_bddNewVar mgr >>= mkBdd mgr

bddIthVar :: Int -> BddIO s (Bdd s)
bddIthVar i
  | i < 0     = error "Cudd.bddIthVar: negative i"
  | otherwise = BddIO $ \mgr -> do
                  cudd_bddIthVar mgr (fromIntegral i) >>= mkBdd mgr


binop :: (DdManager -> DdNode -> DdNode -> IO DdNode)
      -> Bdd s -> Bdd s -> BddIO s (Bdd s)
binop f = \b1 b2 -> BddIO $ \mgr -> do
  -- Be very careful changing these uses of [withForeignPtr]!  The call to
  -- [mkBdd] must occur *outside* the [withForeignPtr] block.
  dd <- withForeignPtr (unBdd b1) $ \pb1 ->
          withForeignPtr (unBdd b2) $ \pb2 -> f mgr pb1 pb2
  mkBdd mgr dd

bddNot :: Bdd s -> BddIO s (Bdd s)
bddNot b = BddIO $ \mgr -> do
  dd <- withForeignPtr (unBdd b) $ \pb -> cudd_Not pb
  mkBdd mgr dd

bddAnd :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddAnd = binop cudd_bddAnd

bddOr :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddOr = binop cudd_bddOr

bddXor :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddXor = binop cudd_bddXor

bddNand :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddNand = binop cudd_bddNand

bddNor :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddNor = binop cudd_bddNor

bddXnor :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddXnor = binop cudd_bddXnor

bddRestrict :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddRestrict = binop cudd_bddRestrict

numVars :: BddIO s Int
numVars = BddIO $ \mgr -> liftM fromIntegral (cudd_ReadSize mgr)
  
numNodes :: BddIO s Int
numNodes = BddIO $ \mgr -> liftM fromIntegral (cudd_ReadNodeCount mgr)

bddSize :: Bdd s -> BddIO s Int
bddSize b = BddIO $ \mgr -> do
  size <- withForeignPtr (unBdd b) cudd_DagSize
  return (fromIntegral size)

bddToBool :: Bdd s -> BddIO s Bool
bddToBool bdd = do
  true  <- bddTrue
  false <- bddFalse    
  case bdd of
    _ | bdd == true  -> return True
      | bdd == false -> return False
      | otherwise    -> error ("Cudd.bddToBool: non-terminal value\n" ++
                               "  true is " ++ show true ++ "\n" ++
                               "  false is " ++ show false ++ "\n" ++
                               "  value is " ++ show bdd ++ "\n")
