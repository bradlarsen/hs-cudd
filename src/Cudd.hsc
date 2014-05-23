{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module Cudd
  ( Mgr
  , newMgr
  , numVars
  , numNodes
  , numNodesAtLevel
  , nodeLimit
  , setNodeLimit

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
  , reorderVariables

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
  , bddIte
  , bddRestrict
  , bddExistAbstract
  , bddUnivAbstract

  , bddCountMinterms
  , VarAssign (..)
  , bddPickOneMinterm

  , bddToDot
  ) where

import ForeignHandle (handleToCFile, fclose)
import Foreign (nullPtr, Ptr, FunPtr, ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CInt(..), CUInt(..), CDouble(..), CChar(..), CLong(..), CString, CFile, withCString)
import Foreign.Marshal (free, mallocArray, peekArray, withArray, withArrayLen, withMany, toBool)

import HsCuddPrelude

import Control.Exception (Exception, throw, bracket)
import System.IO (Handle)
import System.Mem (performGC)

#include "cudd_wrappers.h"



data MgrT
type MgrP = Ptr MgrT
newtype Mgr = Mgr { unMgr :: ForeignPtr MgrT }

withMgr :: Mgr -> (MgrP -> IO a) -> IO a
withMgr = withForeignPtr . unMgr

data DdManagerT
type DdManagerP = Ptr DdManagerT

foreign import ccall "cudd_wrappers.h cw_mgr_ddmanager" cw_mgr_ddmanager
  :: MgrP -> IO DdManagerP
withDdManager :: Mgr -> (DdManagerP -> IO a) -> IO a
withDdManager mgr f = withForeignPtr (unMgr mgr) (cw_mgr_ddmanager >=> f)

foreign import ccall "cudd_wrappers.h cw_init" cw_init
  :: IO MgrP
foreign import ccall "cudd_wrappers.h &cw_quit" cw_quit_p
  :: FunPtr (MgrP -> IO ())
newMgr :: IO Mgr
newMgr = do
  mgr <- cw_init
  ddmanager <- cw_mgr_ddmanager mgr
  let checkRC rc = when (rc /= 1) $ error "failed to add hook"
  preGC <- wrapHook $ \_mgr _str _env -> performGC >> return 1
  checkRC =<< addHook' ddmanager PreGC preGC
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
foreign import ccall "wrapper" wrapHook
  :: HookFun -> IO (FunPtr HookFun)

data HookType = PreGC | PostGC | PreReordering | PostReordering
  deriving (Eq, Ord, Show)

toCudd_HookType :: HookType -> Cudd_HookType
toCudd_HookType ht =
  case ht of
    PreGC          -> cudd_pre_gc_hook
    PostGC         -> cudd_post_gc_hook
    PreReordering  -> cudd_pre_reordering_hook
    PostReordering -> cudd_post_reordering_hook

foreign import ccall "cudd.h Cudd_AddHook" cudd_AddHook
  :: DdManagerP -> FunPtr HookFun -> Cudd_HookType -> IO CInt

addHook' :: DdManagerP -> HookType -> FunPtr HookFun -> IO CInt
addHook' ddmanager ht hf = cudd_AddHook ddmanager hf (toCudd_HookType ht)



foreign import ccall "cudd.h Cudd_ReadSize" cudd_ReadSize
  :: DdManagerP -> IO CInt
numVars :: Mgr -> IO Int
numVars mgr = fromIntegral <$> withDdManager mgr cudd_ReadSize

foreign import ccall "cudd.h Cudd_ReadNodeCount" cudd_ReadNodeCount
  :: DdManagerP -> IO CLong
numNodes :: Mgr -> IO Int
numNodes mgr = fromIntegral <$> withDdManager mgr cudd_ReadNodeCount

foreign import ccall "cudd_wrappers.h cw_mgr_nodes_at_level" cw_mgr_nodes_at_level
  :: MgrP -> CUInt -> IO CUInt
numNodesAtLevel :: Mgr -> Int -> IO Int
numNodesAtLevel mgr lvl = withMgr mgr $ \mgrp ->
  fromIntegral <$> cw_mgr_nodes_at_level mgrp (fromIntegral lvl)

foreign import ccall "cudd.h Cudd_ReadMaxLive" cudd_ReadMaxLive
  :: DdManagerP -> IO CUInt
nodeLimit :: Mgr -> IO Word
nodeLimit mgr = fromIntegral <$> withDdManager mgr cudd_ReadMaxLive

foreign import ccall "cudd.h Cudd_SetMaxLive" cudd_SetMaxLive
  :: DdManagerP -> CUInt -> IO ()
setNodeLimit :: Mgr -> Word -> IO ()
setNodeLimit mgr limit = withDdManager mgr $ \ddmanager -> do
  cudd_SetMaxLive ddmanager (fromIntegral limit)


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

foreign import ccall "cudd.h Cudd_EnableReorderingReporting" cudd_EnableReorderingReporting
  :: DdManagerP -> IO CInt
enableReorderingReporting :: Mgr -> IO ()
enableReorderingReporting mgr = withDdManager mgr $ \ddmanager -> do
  rc <- cudd_EnableReorderingReporting ddmanager
  when (rc /= 1) $ error "Cudd.enableReorderingReporting: call failed"

foreign import ccall "cudd.h Cudd_DisableReorderingReporting" cudd_DisableReorderingReporting
  :: DdManagerP -> IO CInt
disableReorderingReporting :: Mgr -> IO ()
disableReorderingReporting mgr = withDdManager mgr $ \ddmanager -> do
  rc <- cudd_DisableReorderingReporting ddmanager
  when (rc /= 1) $ error "Cudd.disableReorderingReporting: call failed"

foreign import ccall "cudd.h Cudd_ReorderingReporting" cudd_ReorderingReporting
  :: DdManagerP -> IO CInt
reorderingReporting :: Mgr -> IO Bool
reorderingReporting mgr = toBool <$> withDdManager mgr cudd_ReorderingReporting

foreign import ccall "cudd.h Cudd_AutodynEnable" cudd_AutodynEnable
  :: DdManagerP -> Cudd_ReorderingType -> IO ()
enableDynamicReordering :: Mgr -> Cudd_ReorderingType -> IO ()
enableDynamicReordering mgr method = withDdManager mgr $ \ddmanager -> do
  cudd_AutodynEnable ddmanager method

foreign import ccall "cudd.h Cudd_AutodynDisable" cudd_AutodynDisable
  :: DdManagerP -> IO ()
disableDynamicReordering :: Mgr -> IO ()
disableDynamicReordering mgr = withDdManager mgr cudd_AutodynDisable

foreign import ccall "cudd.h Cudd_ShuffleHeap" cudd_ShuffleHeap
  :: DdManagerP -> Ptr CInt -> IO CInt
reorderVariables :: Mgr -> [Int] -> IO ()
reorderVariables mgr permutation = withDdManager mgr $ \ddmanager -> do
  nVars <- fromIntegral <$> cudd_ReadSize ddmanager
  unless (permutation `isPermutationOf` nVars) $ do
    error ("Cudd.reorderVariables: " ++ show permutation ++
           " is not a permutation of [0.." ++ show (nVars - 1) ++ "]")
  withArray (map fromIntegral permutation) $ \permutation' -> do
    res <- cudd_ShuffleHeap ddmanager permutation'
    when (res /= 1) $ do
      error "Cudd.reorderVariables: call failed"



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

foreign import ccall "cudd.h Cudd_ReadErrorCode" cudd_ReadErrorCode
  :: DdManagerP -> IO Cudd_ErrorType

foreign import ccall "cudd.h Cudd_ClearErrorCode" cudd_ClearErrorCode
  :: DdManagerP -> IO ()

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



data BddT
type BddP = Ptr BddT
-- TODO: alas, the default Eq instances (structural equality) are wrong
newtype Bdd = Bdd { unBdd :: ForeignPtr BddT }

data DdNodeT
type DdNodeP = Ptr DdNodeT

withBdd :: Bdd -> (BddP -> IO a) -> IO a
withBdd = withForeignPtr . unBdd

withDdNode :: Bdd -> (DdNodeP -> IO a) -> IO a
withDdNode b f = withBdd b (cw_bdd_ddnode >=> f)

foreign import ccall "cudd_wrappers.h &cw_bdd_destroy" cw_bdd_destroy_p
  :: FunPtr (BddP -> IO ())

mkBdd :: BddP -> IO Bdd
mkBdd bdd = do
  when (bdd == nullPtr) $ do
    err <- cudd_ReadErrorCode =<< cw_bdd_ddmanager bdd
    cudd_ClearErrorCode =<< cw_bdd_ddmanager bdd
    throw (toCuddException err)
  Bdd <$> newForeignPtr cw_bdd_destroy_p bdd

foreign import ccall "cudd_wrappers.h cw_bdd_mgr" cw_bdd_mgr
  :: BddP -> IO MgrP

foreign import ccall "cudd_wrappers.h cw_bdd_ddmanager" cw_bdd_ddmanager
  :: BddP -> IO DdManagerP

foreign import ccall "cudd_wrappers.h cw_bdd_ddnode" cw_bdd_ddnode
  :: BddP -> IO DdNodeP

foreign import ccall "cudd.h Cudd_DagSize" cudd_DagSize
  :: DdNodeP -> IO CInt
bddNumNodes :: Bdd -> IO Int
bddNumNodes bdd = fromIntegral <$> withDdNode bdd cudd_DagSize

-- It is generally wrong to mix BDDs from separate managers; this function
-- checks that two BDDs have the same manager.
checkSameManager :: Bdd -> Bdd -> IO ()
checkSameManager b1 b2 =
  withBdd b1 $ \pb1 -> do
    mgr1 <- cw_bdd_mgr pb1
    withBdd b2 $ \pb2 -> do
      mgr2 <- cw_bdd_mgr pb2
      when (mgr1 /= mgr2) $ error "Cudd.checkSameManager: different managers"

checkAllSameManager :: [Bdd] -> IO ()
checkAllSameManager (b1 : b2 : bs) =
  checkSameManager b1 b2 >> checkAllSameManager (b2 : bs)
checkAllSameManager _ = return ()


bddEqual :: Bdd -> Bdd -> IO Bool
bddEqual b1 b2 = do
  checkSameManager b1 b2
  withDdNode b1 $ \b1p ->
    withDdNode b2 $ \b2p -> return (b1p == b2p)

foreign import ccall "cudd_wrappers.h cw_read_one" cw_read_one
  :: MgrP -> IO BddP
bddTrue :: Mgr -> IO Bdd
bddTrue mgr = withMgr mgr (cw_read_one >=> mkBdd)

foreign import ccall "cudd_wrappers.h cw_read_logic_zero" cw_read_logic_zero
  :: MgrP -> IO BddP
bddFalse :: Mgr -> IO Bdd
bddFalse mgr = withMgr mgr (cw_read_logic_zero >=> mkBdd)

foreign import ccall "cudd_wrappers.h cw_bdd_ith_var" cw_bdd_ith_var
  :: MgrP -> CUInt -> IO BddP
bddIthVar :: Mgr -> Int -> IO Bdd
bddIthVar mgr i
  | i < 0     = error "Cudd.bddIthVar: negative i"
  | otherwise = withMgr mgr $ \mgrp ->
                  cw_bdd_ith_var mgrp (fromIntegral i) >>= mkBdd

binop :: (BddP -> BddP -> IO BddP) -> Bdd -> Bdd -> IO Bdd
binop f = \b1 b2 -> do
  checkSameManager b1 b2
  withBdd b1 $ \b1p -> do
    withBdd b2 $ \b2p -> do
      f b1p b2p >>= mkBdd

ternary :: (BddP -> BddP -> BddP -> IO BddP) -> Bdd -> Bdd -> Bdd -> IO Bdd
ternary f = \b1 b2 b3 -> do
  checkSameManager b1 b2
  checkSameManager b1 b3
  withBdd b1 $ \b1p -> do
    withBdd b2 $ \b2p -> do
      withBdd b3 $ \b3p -> do
        f b1p b2p b3p >>= mkBdd

foreign import ccall "cudd_wrappers.h cw_bdd_not" cw_bdd_not
  :: BddP -> IO BddP
bddNot :: Bdd -> IO Bdd
bddNot b = withBdd b (cw_bdd_not >=> mkBdd)

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

foreign import ccall "cudd_wrappers.h cw_bdd_ite" cw_bdd_ite
  :: BddP -> BddP -> BddP -> IO BddP
bddIte :: Bdd -> Bdd -> Bdd -> IO Bdd
bddIte = ternary cw_bdd_ite

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

foreign import ccall "cudd.h Cudd_CountMinterm" cudd_CountMinterm
  :: DdManagerP -> DdNodeP -> CInt -> IO CDouble
bddCountMinterms :: Bdd -> IO Double
bddCountMinterms b = withBdd b $ \bp -> do
  ddnode <- cw_bdd_ddnode bp
  ddmanager <- cw_bdd_ddmanager bp
  nVars <- cudd_ReadSize ddmanager
  res <- cudd_CountMinterm ddmanager ddnode nVars
  when (res == fromIntegral cUDD_OUT_OF_MEM) $ do
    throw CuddMemoryOut
  return $ realToFrac res


data VarAssign = Positive | Negative | DoNotCare
  deriving (Eq, Ord, Read, Show)

foreign import ccall "cudd.h Cudd_bddPickOneCube" cudd_bddPickOneCube
  :: DdManagerP -> DdNodeP -> Ptr CChar -> IO CInt
bddPickOneMinterm :: Bdd -> IO (Maybe [(Int, VarAssign)])
bddPickOneMinterm b = withBdd b $ \bp -> do
  ddmanager <- cw_bdd_ddmanager bp
  nVars <- fromIntegral <$> cudd_ReadSize ddmanager
  bracket (mallocArray nVars) free $ \arr -> do
    ddnode <- cw_bdd_ddnode bp
    rc <- cudd_bddPickOneCube ddmanager ddnode arr
    if rc == 1
       then do values <- peekArray nVars arr
               liftM Just $ forM (zip [0..] values) $ \(idx, val) -> do
                 case val of
                   0 -> return (idx, Negative)
                   1 -> return (idx, Positive)
                   2 -> return (idx, DoNotCare)
                   _ -> error "Cudd.bddPickOneMinterm: unexpected variable binding"
       else return Nothing


foreign import ccall "cudd.h Cudd_ReadLogicZero" cudd_ReadLogicZero
  :: DdManagerP -> IO DdNodeP
foreign import ccall "cudd.h Cudd_ReadOne" cudd_ReadOne
  :: DdManagerP -> IO DdNodeP
bddToBool :: Bdd -> IO Bool
bddToBool bdd = withBdd bdd $ \bddp -> do
  ddmanager <- cw_bdd_ddmanager bddp
  ddnode <- cw_bdd_ddnode bddp
  isTrue <- (ddnode ==) <$> cudd_ReadOne ddmanager
  if isTrue then return True
     else do isFalse <- (ddnode ==) <$> cudd_ReadLogicZero ddmanager
             if isFalse then return False
                else error "Cudd.bddToBool: non-terminal value"


foreign import ccall "cudd.h Cudd_DumpDot" cudd_DumpDot
  :: DdManagerP -> CInt -> Ptr DdNodeP -> Ptr CString -> Ptr CString
  -> Ptr CFile -> IO CInt

withDdNodeArrayLen :: [Bdd] -> (Int -> Ptr DdNodeP -> IO a) -> IO a
withDdNodeArrayLen bdds f =
  withMany withDdNode bdds $ \ddnodes ->
    withArrayLen ddnodes $ \n arr ->
      f n arr

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withMany withCString ss $ flip withArray f

withCStringArrayLen :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringArrayLen ss f =
  withMany withCString ss $ \ss' -> withArrayLen ss' f

-- TODO: eliminate Mgr argument; check Bdds for same mgr; check input name length
bddToDot :: [(Bdd, String)] -> [String] -> Handle -> IO ()
bddToDot [] _inputNames _hOut = error "Cudd.bddToDot: no BDDs given"
bddToDot roots inputNames hOut = do
  let (bdds, outputNames) = unzip roots
  checkAllSameManager bdds
  withBdd (head bdds) $ \bdd -> do
    ddmanager <- cw_bdd_ddmanager bdd
    withDdNodeArrayLen bdds $ \n ddNodeArr ->
      withCStringArrayLen inputNames $ \inputNameArrLen inputNameArr -> do
        nvs <- fromIntegral <$> cudd_ReadSize ddmanager
        when (inputNameArrLen /= nvs) $ do
          error "Cudd.bddToDot: not enough input names!\n"
        withCStringArray outputNames $ \outputNameArr ->
          bracket (handleToCFile hOut "w") fclose $ \hCFileP -> do
            res <- cudd_DumpDot ddmanager (fromIntegral n) ddNodeArr
                                inputNameArr outputNameArr hCFileP
            when (res /= 1) $ do
              error "Cudd.bddToDot: call failed"


isPermutationOf :: [Int] -> Int -> Bool
isPermutationOf vs n = sort vs == [0..n-1]
