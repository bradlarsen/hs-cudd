{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Cudd.Raw where

import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CInt, CUInt, CULong, CDouble, CChar)

#include <cudd.h>

data BddT
data MgrT

type BddP = Ptr BddT
type MgrP = Ptr MgrT

foreign import ccall "cudd_wrappers.h cw_init" cw_init
  :: IO MgrP

foreign import ccall "cudd_wrappers.h &cw_quit" cw_quit_p
  :: FunPtr (MgrP -> IO ())

foreign import ccall "cudd_wrappers.h &cw_bdd_destroy" cw_bdd_destroy_p
  :: FunPtr (BddP -> IO ())

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

foreign import ccall "cudd_wrappers.h cw_bdd_get_manager" cw_bdd_get_manager
  :: BddP -> IO MgrP

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


foreign import ccall "cudd_wrappers.h cw_read_one" cw_read_one
  :: MgrP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_read_logic_zero" cw_read_logic_zero
  :: MgrP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_ith_var" cw_bdd_ith_var
  :: MgrP -> CUInt -> IO BddP

foreign import ccall "cudd_wrappers.h cw_bdd_is_one" cw_bdd_is_one
  :: BddP -> IO CInt
foreign import ccall "cudd_wrappers.h cw_bdd_is_logic_zero" cw_bdd_is_logic_zero
  :: BddP -> IO CInt

foreign import ccall "cudd_wrappers.h cw_bdd_equal" cw_bdd_equal
  :: BddP -> BddP -> IO CInt

foreign import ccall "cudd_wrappers.h cw_bdd_not" cw_bdd_not
  :: BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_ite" cw_bdd_ite
  :: BddP -> BddP -> BddP -> IO BddP

foreign import ccall "cudd_wrappers.h cw_bdd_and" cw_bdd_and
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_or" cw_bdd_or
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_xor" cw_bdd_xor
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_nand" cw_bdd_nand
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_nor" cw_bdd_nor
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_xnor" cw_bdd_xnor
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_compose" cw_bdd_compose
  :: BddP -> BddP -> CUInt -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_restrict" cw_bdd_restrict
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_exist_abstract" cw_bdd_exist_abstract
  :: BddP -> BddP -> IO BddP
foreign import ccall "cudd_wrappers.h cw_bdd_univ_abstract" cw_bdd_univ_abstract
  :: BddP -> BddP -> IO BddP

foreign import ccall "cudd_wrappers.h cw_bdd_count_minterm" cw_bdd_count_minterm
  :: BddP -> IO CDouble

foreign import ccall "cudd_wrappers.h cw_num_bdd_vars" cw_num_bdd_vars
  :: MgrP -> IO CUInt
foreign import ccall "cudd_wrappers.h cw_num_nodes" cw_num_nodes
  :: MgrP -> IO CUInt
foreign import ccall "cudd_wrappers.h cw_bdd_size" cw_bdd_size
  :: BddP -> IO CUInt

foreign import ccall "cudd_wrappers.h cw_bdd_pick_one_cube" cw_bdd_pick_one_cube
  :: BddP -> Ptr CChar -> IO CInt
