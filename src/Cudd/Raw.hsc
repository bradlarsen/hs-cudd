{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Cudd.Raw where

import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CInt, CUInt, CULong)

#include <cudd.h>



data DdManagerT
type DdManager = Ptr DdManagerT

foreign import ccall "cudd.h Cudd_Init" cudd_Init
  :: CUInt        -- ^ number of BDD and ADD variables
  -> CUInt        -- ^ number of ZDD variables
  -> CUInt        -- ^ initial size of each subtable of the unique table
  -> CUInt        -- ^ initial number of entries in the cache
  -> CULong       -- ^ target number of bytes for max memory occupation
  -> IO DdManager

-- | The default size of each subtable of the unique table
cudd_unique_slots :: CUInt
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

-- | The default number of entries in the cache
cudd_cache_slots :: CUInt
cudd_cache_slots = #const CUDD_CACHE_SLOTS

foreign import ccall "cudd.h Cudd_Quit" cudd_Quit
  :: DdManager -> IO ()

foreign import ccall "cudd_wrappers.h Cudd_Quit_Wrapper" cudd_Quit_Wrapper
  :: DdManager -> IO ()

foreign import ccall "cudd_wrappers.h &Cudd_RecursiveDeref_Wrapper" cudd_RecursiveDeref_Wrapper_p
  :: FunPtr (DdManager -> DdNode -> IO ())




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

foreign import ccall "cudd.h Cudd_ReadErrorCode" cudd_ReadErrorCode
  :: DdManager
  -> IO Cudd_ErrorType

foreign import ccall "cudd.h Cudd_ClearErrorCode" cudd_ClearErrorCode
  :: DdManager
  -> IO ()




data DdNodeT
type DdNode = Ptr DdNodeT

-- | Returns logic 1.
foreign import ccall "cudd.h Cudd_ReadOne" cudd_ReadOne
  :: DdManager
  -> IO DdNode

-- | Returns logic 0.
foreign import ccall "cudd.h Cudd_ReadLogicZero" cudd_ReadLogicZero
  :: DdManager
  -> IO DdNode

-- | Returns a new projection function, whose index is the largest index in use
-- at the time of the call, plus 1.
foreign import ccall "cudd.h Cudd_bddNewVar" cudd_bddNewVar
  :: DdManager
  -> IO DdNode

-- | Returns the projection function with index i. If the function does not
-- exist, it is created.
foreign import ccall "cudd.h Cudd_bddIthVar" cudd_bddIthVar
  :: DdManager
  -> CInt
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddIte" cudd_bddIte
  :: DdManager
  -> DdNode
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddAnd" cudd_bddAnd
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddOr" cudd_bddOr
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddXor" cudd_bddXor
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddNand" cudd_bddNand
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddNor" cudd_bddNor
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

foreign import ccall "cudd.h Cudd_bddXnor" cudd_bddXnor
  :: DdManager
  -> DdNode
  -> DdNode
  -> IO DdNode

-- | Substitutes g for x_v in the BDD for f.
foreign import ccall "cudd.h Cudd_bddCompose" cudd_bddCompose
  :: DdManager
  -> DdNode       -- ^ f
  -> DdNode       -- ^ g
  -> CInt         -- ^ v
  -> IO DdNode

-- | Complements a DdNode.
foreign import ccall "cudd_wrappers.h Cudd_Not_Wrapper" cudd_Not
  :: DdNode
  -> IO DdNode

-- | f if g
foreign import ccall "cudd.h Cudd_bddRestrict" cudd_bddRestrict
  :: DdManager
  -> DdNode       -- ^ f, the BDD to restrict
  -> DdNode       -- ^ g, the BDD to restrict by
  -> IO DdNode




foreign import ccall "cudd.h Cudd_Ref" cudd_Ref
  :: DdNode
  -> IO ()

foreign import ccall "cudd.h Cudd_RecursiveDeref" cudd_RecursiveDeref
  :: DdManager
  -> DdNode
  -> IO ()

foreign import ccall "cudd.h &Cudd_RecursiveDeref" cudd_RecursiveDeref_p
  :: FunPtr (DdManager -> DdNode -> IO ())




-- | Returns the number of BDD variables in the manager.
foreign import ccall "cudd.h Cudd_ReadSize" cudd_ReadSize
  :: DdManager
  -> IO CInt

-- | Returns the number of nodes in the manager.
foreign import ccall "cudd.h Cudd_ReadNodeCount" cudd_ReadNodeCount
  :: DdManager
  -> IO CInt

-- | Returns the number of nodes in the given BDD.
foreign import ccall "cudd.h Cudd_DagSize" cudd_DagSize
  :: DdNode -> IO CInt


foreign import ccall "cudd.h Cudd_GarbageCollectionEnabled"
  cudd_GarbageCollectionEnabled
  :: DdManager -> IO CInt

foreign import ccall "cudd.h Cudd_EnableGarbageCollection"
  cudd_EnableGarbageCollection
  :: DdManager -> IO ()

foreign import ccall "cudd.h Cudd_DisableGarbageCollection"
  cudd_DisableGarbageCollection
  :: DdManager -> IO ()
