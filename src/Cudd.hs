-- This module provides a higher-level interface to Cudd.Raw.  In particular,
-- this interface hides the DdManager type; a single manager is implicit in
-- every BddIO computation.  This interface uses a type of rank 2 to ensure
-- that no reference to a manager escapes.
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
  , bddToBool
  ) where

import Cudd.Raw

import Foreign (nullPtr)

import Control.Exception (Exception, finally, throw)
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)

newtype Bdd s = Bdd { unBdd :: DdNode }
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
mkBdd mgr dd@(DdNode ptr) = do
  when (ptr == nullPtr) $ do
    err <- cudd_ReadErrorCode mgr
    throw (toCuddException err)
  cudd_Ref dd
  return $ Bdd dd



newDefaultManager :: IO DdManager
newDefaultManager = cudd_Init 0 0 cudd_unique_slots cudd_cache_slots 0

runBddIO :: (forall s. BddIO s a) -> IO a
runBddIO f = do
  mgr <- newDefaultManager
  unBddIO f mgr `finally` cudd_Quit mgr
  

bddTrue :: BddIO s (Bdd s)
bddTrue = BddIO $ \mgr -> do
  dd <- cudd_ReadOne mgr
  mkBdd mgr dd

bddFalse :: BddIO s (Bdd s)
bddFalse = BddIO $ \mgr -> do
  dd <- cudd_ReadLogicZero mgr
  mkBdd mgr dd

bddNewVar :: BddIO s (Bdd s)
bddNewVar = BddIO $ \mgr -> do
  dd <- cudd_bddNewVar mgr
  mkBdd mgr dd

bddIthVar :: Int -> BddIO s (Bdd s)
bddIthVar i
  | i < 0     = error "Cudd.bddIthVar: negative i"
  | otherwise = BddIO $ \mgr -> do
                  dd <- cudd_bddIthVar mgr (fromIntegral i)
                  mkBdd mgr dd


binop :: (DdManager -> DdNode -> DdNode -> IO DdNode)
      -> Bdd s -> Bdd s -> BddIO s (Bdd s)
binop f = \b1 b2 -> BddIO $ \mgr -> do
  dd <- f mgr (unBdd b1) (unBdd b2)
  mkBdd mgr dd

bddNot :: Bdd s -> BddIO s (Bdd s)
bddNot b = BddIO $ \mgr -> do
  dd <- cudd_Not (unBdd b)
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
