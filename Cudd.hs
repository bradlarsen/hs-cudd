-- This module provides a higher-level interface to Cudd.Raw.  In particular,
-- this interface hides the DdManager type; a single manager is implicit in
-- every BddIO computation.  This interface uses a type of rank 2 to ensure
-- that no reference to a manager escapes.
{-# LANGUAGE Rank2Types #-}
module Cudd
  ( BddIO
  , runBddIO
  , Bdd
  , bddTrue
  , bddFalse
  , bddAnd
  ) where

import Cudd.Raw

import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype Bdd s = Bdd { unBdd :: DdNode }
  deriving (Eq, Show)


newtype BddIO s a = BddIO { unBddIO :: DdManager -> IO a }

instance Monad (BddIO s) where
  return a = BddIO $ \_ -> return a
  m >>= f = BddIO $ \mgr -> do
              v <- unBddIO m mgr
              unBddIO (f v) mgr

instance MonadIO (BddIO s) where
  liftIO a = BddIO $ \_ -> a


newDefaultManager :: IO DdManager
newDefaultManager = cudd_Init 0 0 cudd_unique_slots cudd_cache_slots 0

runBddIO :: (forall s. BddIO s a) -> IO a
runBddIO f = do
  mgr <- newDefaultManager
  unBddIO f mgr `finally` cudd_Quit mgr
  
bddTrue :: BddIO s (Bdd s)
bddTrue = BddIO $ \mgr -> do
  dd <- cudd_ReadOne mgr
  return $ Bdd dd

bddFalse :: BddIO s (Bdd s)
bddFalse = BddIO $ \mgr -> do
  dd <- cudd_ReadZero mgr
  return $ Bdd dd

bddAnd :: Bdd s -> Bdd s -> BddIO s (Bdd s)
bddAnd b1 b2 = BddIO $ \mgr -> do
  dd <- cudd_bddAnd mgr (unBdd b1) (unBdd b2)
  return $ Bdd dd
