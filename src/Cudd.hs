{-# LANGUAGE DeriveDataTypeable #-}
module Cudd
  ( CuddException (..)
  , Mgr
  , newMgr
  , Bdd
  , bddEqual
  , bddTrue
  , bddFalse
  , bddIthVar
  , bddNot
  , bddAnd
  , bddOr
  , bddXor
  , bddNand
  , bddNor
  , bddXnor
  , bddRestrict
  , bddExistAbstract
  , bddUnivAbstract
  , bddCountMinterms
  , numVars
  , numNodes
  , bddSize
  , bddToBool
  ) where

import Cudd.Raw

import Foreign (nullPtr, Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr, newForeignPtrEnv)

import Control.Exception (Exception, finally, throw)
import Control.Monad ((>=>), when, liftM)
import Data.Typeable (Typeable)





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




-- The reference-counting wrappers around the CUDD types
-- alas, the default Eq instances are wrong due to the extra level of
-- indirection from reference counting.
-- TODO:  make these newtypes
type Bdd = (ForeignPtr BddT)
type Mgr = (ForeignPtr MgrT)

mkBdd :: MgrP -> BddP -> IO Bdd
mkBdd mgr bdd = do
  when (bdd == nullPtr) $ do
    err <- cw_read_error_code mgr
    throw (toCuddException err)
  newForeignPtr cw_bdd_destroy_p bdd

newMgr :: IO Mgr
newMgr = cw_init >>= newForeignPtr cw_quit_p

bddEqual :: Bdd -> Bdd -> IO Bool
bddEqual b1 b2 = do
  checkSameManager b1 b2
  withForeignPtr b1 $ \b1p ->
    withForeignPtr b2 $ \b2p ->
      liftM (/= 0) (cw_bdd_equal b1p b2p)

bddTrue :: Mgr -> IO Bdd
bddTrue mgr =
  withForeignPtr mgr $ \mgrp ->
    (cw_read_one mgrp >>= mkBdd mgrp)

bddFalse :: Mgr -> IO Bdd
bddFalse mgr =
  withForeignPtr mgr $ \mgrp ->
    (cw_read_logic_zero mgrp >>= mkBdd mgrp)

bddIthVar :: Mgr -> Int -> IO Bdd
bddIthVar mgr i
  | i < 0     = error "Cudd.bddIthVar: negative i"
  | otherwise = withForeignPtr mgr $ \mgrp ->
                  (cw_bdd_ith_var mgrp (fromIntegral i) >>= mkBdd mgrp)

checkSameManager :: Bdd -> Bdd -> IO ()
checkSameManager b1 b2 =
  withForeignPtr b1 $ \pb1 -> do
    mgr1 <- cw_bdd_get_manager pb1
    withForeignPtr b2 $ \pb2 -> do
      mgr2 <- cw_bdd_get_manager pb2
      when (mgr1 /= mgr2) $ error "Cudd.checkSameManager: different managers!"

binop :: (BddP -> BddP -> IO BddP) -> Bdd -> Bdd -> IO Bdd
binop f = \b1 b2 -> do
  checkSameManager b1 b2
  mgr <- withForeignPtr b1 cw_bdd_get_manager
  bdd <- withForeignPtr b1 $ \pb1 ->
           withForeignPtr b2 $ \pb2 ->
             f pb1 pb2
  mkBdd mgr bdd

bddNot :: Bdd -> IO Bdd
bddNot b = do
  bMgr <- withForeignPtr b cw_bdd_get_manager
  b' <- withForeignPtr b cw_bdd_not
  mkBdd bMgr b'

bddAnd :: Bdd -> Bdd -> IO Bdd
bddAnd = binop cw_bdd_and

bddOr :: Bdd -> Bdd -> IO Bdd
bddOr = binop cw_bdd_or

bddXor :: Bdd -> Bdd -> IO Bdd
bddXor = binop cw_bdd_xor

bddNand :: Bdd -> Bdd -> IO Bdd
bddNand = binop cw_bdd_nand

bddNor :: Bdd -> Bdd -> IO Bdd
bddNor = binop cw_bdd_nor

bddXnor :: Bdd -> Bdd -> IO Bdd
bddXnor = binop cw_bdd_xnor

bddRestrict :: Bdd -> Bdd -> IO Bdd
bddRestrict = binop cw_bdd_restrict

bddExistAbstract :: Bdd -> Bdd -> IO Bdd
bddExistAbstract = binop cw_bdd_exist_abstract

bddUnivAbstract :: Bdd -> Bdd -> IO Bdd
bddUnivAbstract = binop cw_bdd_univ_abstract

bddCountMinterms :: Bdd -> IO Double
bddCountMinterms b = do
  res <- withForeignPtr b cw_bdd_count_minterm
  when (res == fromIntegral cUDD_OUT_OF_MEM) $ do
    throw CuddMemoryOut
  return $ realToFrac res

numVars :: Mgr -> IO Int
numVars mgr = liftM fromIntegral $ withForeignPtr mgr cw_num_bdd_vars

numNodes :: Mgr -> IO Int
numNodes mgr = liftM fromIntegral $ withForeignPtr mgr cw_num_nodes

bddSize :: Bdd -> IO Int
bddSize bdd = liftM fromIntegral $ withForeignPtr bdd cw_bdd_size

bddToBool :: Bdd -> IO Bool
bddToBool bdd = do
  isTrue <- withForeignPtr bdd cw_bdd_is_one
  if isTrue /= 0 then return True
     else do isFalse <- withForeignPtr bdd cw_bdd_is_logic_zero
             if isFalse /= 0 then return False
                else error "Cudd.bddToBool: non-terminal value"
