{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Cudd
  ( Mgr
  , newMgr
  , numVars
  , numNodes

  , CuddException (..)

  , enableReorderingReporting
  , disableReorderingReporting
  , reorderingReporting
  , enableDynamicReordering
  , disableDynamicReordering
  , Cudd_ReorderingType
  , cudd_reorder_same
  , cudd_reorder_none
  , cudd_reorder_random
  , cudd_reorder_random_pivot
  , cudd_reorder_sift
  , cudd_reorder_sift_converge
  , cudd_reorder_symm_sift
  , cudd_reorder_symm_sift_conv
  , cudd_reorder_window2
  , cudd_reorder_window3
  , cudd_reorder_window4
  , cudd_reorder_window2_conv
  , cudd_reorder_window3_conv
  , cudd_reorder_window4_conv
  , cudd_reorder_group_sift
  , cudd_reorder_group_sift_conv
  , cudd_reorder_annealing
  , cudd_reorder_genetic
  , cudd_reorder_linear
  , cudd_reorder_linear_converge
  , cudd_reorder_lazy_sift
  , cudd_reorder_exac

  , Bdd
  , bddNumNodes
  , bddToBool

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
  , VarAssign (..)
  , bddPickOneMinterm
  ) where

import Foreign (nullPtr, Ptr, FunPtr)
import Foreign.C.Types (CInt, CUInt, CDouble, CChar)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, peekArray)

import Control.Applicative ((<$>))
import Control.Exception (Exception, throw, bracket)
import Control.Monad ((>=>), when, liftM, forM)
import Data.Typeable (Typeable)

import System.IO (stderr)
import System.Mem (performGC)
import Text.Printf (hPrintf)

#include "cudd_wrappers.h"



data MgrT
type MgrP = Ptr MgrT
newtype Mgr = Mgr { unMgr :: ForeignPtr MgrT }

withMgr :: Mgr -> (MgrP -> IO a) -> IO a
withMgr = withForeignPtr . unMgr

foreign import ccall "cudd_wrappers.h cw_init" cw_init
  :: IO MgrP
foreign import ccall "cudd_wrappers.h &cw_quit" cw_quit_p
  :: FunPtr (MgrP -> IO ())
newMgr :: IO Mgr
newMgr = do
  mgr <- cw_init
  let checkRC rc = when (rc /= 1) $ error "failed to add hook"
  let numNodes :: IO Int
      numNodes = fromIntegral <$> cw_num_nodes mgr
  preGC  <- wrapHook $ \_mgr _str _env -> do
              hPrintf stderr "CUDD garbage collection: %d --> " =<< numNodes
              performGC
              return 1
  cw_add_hook mgr preGC cudd_pre_gc_hook >>= checkRC
  postGC <- wrapHook $ \_mgr _str _env -> do
              hPrintf stderr "%d nodes\n" =<< numNodes
              return 1
  cw_add_hook mgr postGC cudd_post_gc_hook >>= checkRC
  -- TODO:  we should call [freeHaskellFunPtr] on the callbacks when finished
  Mgr <$> newForeignPtr cw_quit_p mgr


newtype Cudd_HookType = Cudd_HookType CInt
  deriving (Eq, Ord)
#{enum Cudd_HookType, Cudd_HookType
 , cudd_pre_gc_hook          = CUDD_PRE_GC_HOOK
 , cudd_post_gc_hook         = CUDD_POST_GC_HOOK
 , cudd_pre_reordering_hook  = CUDD_PRE_REORDERING_HOOK
 , cudd_post_reordering_hook = CUDD_POST_REORDERING_HOOK
 }

-- typedef int (*DD_HFP)(DdManager *, const char *, void *);
type HookFun = Ptr () -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "cudd_wrappers.h cw_add_hook" cw_add_hook
  :: MgrP -> FunPtr HookFun -> Cudd_HookType -> IO CInt

foreign import ccall "wrapper" wrapHook
  :: HookFun -> IO (FunPtr HookFun)

foreign import ccall "cudd_wrappers.h cw_num_bdd_vars" cw_num_bdd_vars
  :: MgrP -> IO CUInt
numVars :: Mgr -> IO Int
numVars mgr = fromIntegral <$> withMgr mgr cw_num_bdd_vars

foreign import ccall "cudd_wrappers.h cw_num_nodes" cw_num_nodes
  :: MgrP -> IO CUInt
numNodes :: Mgr -> IO Int
numNodes mgr = fromIntegral <$> withMgr mgr cw_num_nodes


newtype Cudd_ReorderingType = Cudd_ReorderingType CInt
  deriving (Eq, Ord)
#{enum Cudd_ReorderingType, Cudd_ReorderingType
 , cudd_reorder_same            = CUDD_REORDER_SAME
 , cudd_reorder_none            = CUDD_REORDER_NONE
 , cudd_reorder_random          = CUDD_REORDER_RANDOM
 , cudd_reorder_random_pivot    = CUDD_REORDER_RANDOM_PIVOT
 , cudd_reorder_sift            = CUDD_REORDER_SIFT
 , cudd_reorder_sift_converge   = CUDD_REORDER_SIFT_CONVERGE
 , cudd_reorder_symm_sift       = CUDD_REORDER_SYMM_SIFT
 , cudd_reorder_symm_sift_conv  = CUDD_REORDER_SYMM_SIFT_CONV
 , cudd_reorder_window2         = CUDD_REORDER_WINDOW2
 , cudd_reorder_window3         = CUDD_REORDER_WINDOW3
 , cudd_reorder_window4         = CUDD_REORDER_WINDOW4
 , cudd_reorder_window2_conv    = CUDD_REORDER_WINDOW2_CONV
 , cudd_reorder_window3_conv    = CUDD_REORDER_WINDOW3_CONV
 , cudd_reorder_window4_conv    = CUDD_REORDER_WINDOW4_CONV
 , cudd_reorder_group_sift      = CUDD_REORDER_GROUP_SIFT
 , cudd_reorder_group_sift_conv = CUDD_REORDER_GROUP_SIFT_CONV
 , cudd_reorder_annealing       = CUDD_REORDER_ANNEALING
 , cudd_reorder_genetic         = CUDD_REORDER_GENETIC
 , cudd_reorder_linear          = CUDD_REORDER_LINEAR
 , cudd_reorder_linear_converge = CUDD_REORDER_LINEAR_CONVERGE
 , cudd_reorder_lazy_sift       = CUDD_REORDER_LAZY_SIFT
 , cudd_reorder_exac            = CUDD_REORDER_EXACT
 }

foreign import ccall "cudd_wrappers.h cw_enable_reordering_reporting" cw_enable_reordering_reporting
  :: MgrP -> IO CInt
enableReorderingReporting :: Mgr -> IO ()
enableReorderingReporting mgr = withMgr mgr $ \mgr -> do
  rc <- cw_enable_reordering_reporting mgr
  when (rc /= 1) $ error "Cudd.enableReorderingReporting: call failed"

foreign import ccall "cudd_wrappers.h cw_disable_reordering_reporting" cw_disable_reordering_reporting
  :: MgrP -> IO CInt
disableReorderingReporting :: Mgr -> IO ()
disableReorderingReporting mgr = withMgr mgr $ \mgr -> do
  rc <- cw_disable_reordering_reporting mgr
  when (rc /= 1) $ error "Cudd.disableReorderingReporting: call failed"

foreign import ccall "cudd_wrappers.h cw_reordering_reporting" cw_reordering_reporting
  :: MgrP -> IO CInt
reorderingReporting :: Mgr -> IO Bool
reorderingReporting mgr = cintToBool <$> withMgr mgr cw_reordering_reporting

foreign import ccall "cudd_wrappers.h cw_autodyn_enable" cw_autodyn_enable
  :: MgrP -> Cudd_ReorderingType -> IO ()
enableDynamicReordering :: Mgr -> Cudd_ReorderingType -> IO ()
enableDynamicReordering mgr method = withMgr mgr $ \mgr -> do
  cw_autodyn_enable mgr method

foreign import ccall "cudd_wrappers.h cw_autodyn_disable" cw_autodyn_disable
  :: MgrP -> IO ()
disableDynamicReordering :: Mgr -> IO ()
disableDynamicReordering mgr = withMgr mgr cw_autodyn_disable



data BddT
type BddP = Ptr BddT
-- TODO: alas, the default Eq instances (structural equality) are wrong
newtype Bdd = Bdd { unBdd :: ForeignPtr BddT }

withBdd :: Bdd -> (BddP -> IO a) -> IO a
withBdd = withForeignPtr . unBdd

foreign import ccall "cudd_wrappers.h &cw_bdd_destroy" cw_bdd_destroy_p
  :: FunPtr (BddP -> IO ())

mkBdd :: MgrP -> BddP -> IO Bdd
mkBdd mgr bdd = do
  when (bdd == nullPtr) $ do
    err <- cw_read_error_code mgr
    throw (toCuddException err)
  Bdd <$> newForeignPtr cw_bdd_destroy_p bdd

foreign import ccall "cudd_wrappers.h cw_bdd_get_manager" cw_bdd_get_manager
  :: BddP -> IO MgrP

foreign import ccall "cudd_wrappers.h cw_bdd_size" cw_bdd_size
  :: BddP -> IO CUInt
bddNumNodes :: Bdd -> IO Int
bddNumNodes bdd = fromIntegral <$> withBdd bdd cw_bdd_size

-- It is generally wrong to mix BDDs from separate managers; this function
-- checks that two BDDs have the same manager.
checkSameManager :: Bdd -> Bdd -> IO ()
checkSameManager b1 b2 =
  withBdd b1 $ \pb1 -> do
    mgr1 <- cw_bdd_get_manager pb1
    withBdd b2 $ \pb2 -> do
      mgr2 <- cw_bdd_get_manager pb2
      when (mgr1 /= mgr2) $ error "Cudd.checkSameManager: different managers"



newtype Cudd_ErrorType = Cudd_ErrorType CInt
  deriving (Eq, Ord)
#{enum Cudd_ErrorType, Cudd_ErrorType
 , cudd_no_error            = CUDD_NO_ERROR
 , cudd_memory_out          = CUDD_MEMORY_OUT
 , cudd_too_many_nodes      = CUDD_TOO_MANY_NODES
 , cudd_max_mem_exceeded    = CUDD_MAX_MEM_EXCEEDED
 , cudd_timeout_expired     = CUDD_TIMEOUT_EXPIRED
 , cudd_invalid_arg         = CUDD_INVALID_ARG
 , cudd_internal_error      = CUDD_INTERNAL_ERROR
 }

cUDD_OUT_OF_MEM :: CInt
cUDD_OUT_OF_MEM = #const CUDD_OUT_OF_MEM

foreign import ccall "cudd_wrappers.h cw_read_error_code" cw_read_error_code
  :: MgrP -> IO Cudd_ErrorType

foreign import ccall "cudd_wrappers.h cw_clear_error_code" cw_clear_error_code
  :: MgrP -> IO ()

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
  | err == cudd_invalid_arg      = CuddInvalidArg
  | err == cudd_internal_error   = CuddInternalError
  | otherwise = error "Cudd.Raw.toCuddException: bad Cudd_ErrorType!"



foreign import ccall "cudd_wrappers.h cw_bdd_equal" cw_bdd_equal
  :: BddP -> BddP -> IO CInt
bddEqual :: Bdd -> Bdd -> IO Bool
bddEqual b1 b2 = do
  checkSameManager b1 b2
  withBdd b1 $ \b1p ->
    withBdd b2 $ \b2p ->
      liftM (/= 0) (cw_bdd_equal b1p b2p)

foreign import ccall "cudd_wrappers.h cw_read_one" cw_read_one
  :: MgrP -> IO BddP
bddTrue :: Mgr -> IO Bdd
bddTrue mgr =
  withMgr mgr $ \mgr ->
    (cw_read_one mgr >>= mkBdd mgr)

foreign import ccall "cudd_wrappers.h cw_read_logic_zero" cw_read_logic_zero
  :: MgrP -> IO BddP
bddFalse :: Mgr -> IO Bdd
bddFalse mgr =
  withMgr mgr $ \mgr ->
    (cw_read_logic_zero mgr >>= mkBdd mgr)

foreign import ccall "cudd_wrappers.h cw_bdd_ith_var" cw_bdd_ith_var
  :: MgrP -> CUInt -> IO BddP
bddIthVar :: Mgr -> Int -> IO Bdd
bddIthVar mgr i
  | i < 0     = error "Cudd.bddIthVar: negative i"
  | otherwise = withMgr mgr $ \mgr ->
                  (cw_bdd_ith_var mgr (fromIntegral i) >>= mkBdd mgr)

binop :: (BddP -> BddP -> IO BddP) -> Bdd -> Bdd -> IO Bdd
binop f = \b1 b2 -> do
  checkSameManager b1 b2
  withBdd b1 $ \b1 -> do
    withBdd b2 $ \b2 -> do
      mgr <- cw_bdd_get_manager b1
      mkBdd mgr =<< f b1 b2

foreign import ccall "cudd_wrappers.h cw_bdd_not" cw_bdd_not
  :: BddP -> IO BddP
bddNot :: Bdd -> IO Bdd
bddNot b = withBdd b $ \b -> do
  mgr <- cw_bdd_get_manager b
  mkBdd mgr =<< cw_bdd_not b

foreign import ccall "cudd_wrappers.h cw_bdd_and" cw_bdd_and
  :: BddP -> BddP -> IO BddP
bddAnd :: Bdd -> Bdd -> IO Bdd
bddAnd = binop cw_bdd_and

foreign import ccall "cudd_wrappers.h cw_bdd_or" cw_bdd_or
  :: BddP -> BddP -> IO BddP
bddOr :: Bdd -> Bdd -> IO Bdd
bddOr = binop cw_bdd_or

foreign import ccall "cudd_wrappers.h cw_bdd_xor" cw_bdd_xor
  :: BddP -> BddP -> IO BddP
bddXor :: Bdd -> Bdd -> IO Bdd
bddXor = binop cw_bdd_xor

foreign import ccall "cudd_wrappers.h cw_bdd_nand" cw_bdd_nand
  :: BddP -> BddP -> IO BddP
bddNand :: Bdd -> Bdd -> IO Bdd
bddNand = binop cw_bdd_nand

foreign import ccall "cudd_wrappers.h cw_bdd_nor" cw_bdd_nor
  :: BddP -> BddP -> IO BddP
bddNor :: Bdd -> Bdd -> IO Bdd
bddNor = binop cw_bdd_nor

foreign import ccall "cudd_wrappers.h cw_bdd_xnor" cw_bdd_xnor
  :: BddP -> BddP -> IO BddP
bddXnor :: Bdd -> Bdd -> IO Bdd
bddXnor = binop cw_bdd_xnor

foreign import ccall "cudd_wrappers.h cw_bdd_restrict" cw_bdd_restrict
  :: BddP -> BddP -> IO BddP
bddRestrict :: Bdd -> Bdd -> IO Bdd
bddRestrict = binop cw_bdd_restrict

foreign import ccall "cudd_wrappers.h cw_bdd_exist_abstract" cw_bdd_exist_abstract
  :: BddP -> BddP -> IO BddP
bddExistAbstract :: Bdd -> Bdd -> IO Bdd
bddExistAbstract = binop cw_bdd_exist_abstract

foreign import ccall "cudd_wrappers.h cw_bdd_univ_abstract" cw_bdd_univ_abstract
  :: BddP -> BddP -> IO BddP
bddUnivAbstract :: Bdd -> Bdd -> IO Bdd
bddUnivAbstract = binop cw_bdd_univ_abstract

foreign import ccall "cudd_wrappers.h cw_bdd_count_minterm" cw_bdd_count_minterm
  :: BddP -> IO CDouble
bddCountMinterms :: Bdd -> IO Double
bddCountMinterms b = do
  res <- withBdd b cw_bdd_count_minterm
  when (res == fromIntegral cUDD_OUT_OF_MEM) $ do
    throw CuddMemoryOut
  return $ realToFrac res


data VarAssign = Positive | Negative | DoNotCare
  deriving (Eq, Ord, Read, Show)

foreign import ccall "cudd_wrappers.h cw_bdd_pick_one_cube" cw_bdd_pick_one_cube
  :: BddP -> Ptr CChar -> IO CInt
bddPickOneMinterm :: Bdd -> IO (Maybe [(Int, VarAssign)])
bddPickOneMinterm b = withBdd b $ \b -> do
  mgr <- cw_bdd_get_manager b
  numVars <- fromIntegral <$> cw_num_bdd_vars mgr
  bracket (mallocArray numVars) free $ \arr -> do
    rc <- cw_bdd_pick_one_cube b arr
    if rc == 1
       then do values <- peekArray numVars arr
               liftM Just $ forM (zip [0..] values) $ \(idx, val) -> do
                 case val of
                   0 -> return (idx, Negative)
                   1 -> return (idx, Positive)
                   2 -> return (idx, DoNotCare)
                   _ -> error "Cudd.bddPickOneMinterm: unexpected variable binding"
       else return Nothing


foreign import ccall "cudd_wrappers.h cw_bdd_is_one" cw_bdd_is_one
  :: BddP -> IO CInt
foreign import ccall "cudd_wrappers.h cw_bdd_is_logic_zero" cw_bdd_is_logic_zero
  :: BddP -> IO CInt
bddToBool :: Bdd -> IO Bool
bddToBool bdd = do
  isTrue <- withBdd bdd cw_bdd_is_one
  if isTrue /= 0 then return True
     else do isFalse <- withBdd bdd cw_bdd_is_logic_zero
             if isFalse /= 0 then return False
                else error "Cudd.bddToBool: non-terminal value"


cintToBool :: CInt -> Bool
cintToBool 0 = False
cintToBool _ = True
