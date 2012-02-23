-- http://hackage.haskell.org/packages/archive/mtp/latest/doc/html/src/Foreign-Handle.html
{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignHandle (
    fdopen, fflush, fclose,
    handleToCFile
    ) where

import GHC.IO.Handle (Handle, hDuplicate)
import Foreign (Ptr)
import Foreign.C (CString, CFile, withCAString)
import System.Posix.IO (handleToFd)
import System.Posix.Types (Fd)

foreign import ccall unsafe "stdio.h fdopen" fdopen
  :: Fd -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "stdio.h fflush" fflush
  :: Ptr CFile -> IO ()

foreign import ccall unsafe "stdio.h fclose" fclose
  :: Ptr CFile -> IO ()

-- | Convert a Handle to a CFile.
-- Source <http://haskell.org/haskellwiki/The_Monad.Reader/Issue2/Bzlib2Binding>
handleToCFile :: Handle -> String -> IO (Ptr CFile)
handleToCFile h m = withCAString m $ \iomode -> do
    -- Create a duplicate so the original handle is kept open
    h' <- hDuplicate h
    fd <- handleToFd h'
    fdopen fd iomode
