{-# LANGUAGE ForeignFunctionInterface #-}
-- | Win32 system-dependent code for 'TransmitFile'.
module Network.Socket.SendFile.Win32 (_sendFile, sendFileIter) where
import Data.Bits ((.|.))
import Data.Int
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (IntPtr(..), Ptr, intPtrToPtr, nullPtr)
import Foreign.Storable (peek)
import Network.Socket.SendFile.Iter (Iter(..),runIter)
import System.Posix.Types (Fd(..))
import System.Win32.Types (DWORD, HANDLE, failIfZero)

#include <mswsock.h>
#include <windows.h>

type SOCKET = Fd

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count =
    do _ <- runIter (sendFileIter out_fd in_fd (fromIntegral count) (fromIntegral off) (fromIntegral count)) -- set blockSize == count. ie. send it all if we can.
       return ()

sendFileIter  :: Fd -- ^ file descriptor corresponding to network socket
              -> Fd -- ^ file descriptor corresponding to file
              -> Int64 -- ^ maximum number of bytes to send at once
              -> Int64 -- ^ offset into file
              -> Int64 -- ^ total number of bytes to send
              -> IO Iter
sendFileIter out_fd in_fd blockSize off count =
    do in_hdl <- get_osfhandle in_fd
       sendFileIterI out_fd in_hdl (min (fromIntegral blockSize) maxBytes) (fromIntegral off) (fromIntegral count)

sendFileIterI :: SOCKET -- ^ file descriptor corresponding to network socket
              -> HANDLE -- ^ file descriptor corresponding to file
              -> Int64 -- ^ maximum number of bytes to send at once
              -> Int64 -- ^ offset into file
              -> Int64 -- ^ total number of bytes to send
              -> IO Iter
sendFileIterI _out_fd _in_fd _blockSize _off  0         = return (Done 0)
sendFileIterI  out_fd  in_fd  blockSize  off  remaining =
    do let bytes = min remaining blockSize
       (wouldBlock, nsent) <- sendfileI out_fd in_fd off bytes
       let cont = sendFileIterI out_fd in_fd blockSize (off + nsent) (remaining `safeMinus` (fromIntegral nsent))
       case wouldBlock of
         True  -> return (WouldBlock (fromIntegral nsent) out_fd cont)
         False -> return (Sent (fromIntegral nsent) cont)

get_osfhandle :: Fd        -- ^ User file descriptor.
              -> IO HANDLE -- ^ The operating-system file handle.
get_osfhandle fd = do
    res <- throwErrnoIf
             (== (#const INVALID_HANDLE_VALUE))
             "Network.Socket.SendFile.Win32.get_osfhandle"
             (c_get_osfhandle fd)
    return (intPtrToPtr res)

setFilePointerEx
    :: HANDLE    -- ^ the handle to set the pointer on
    -> Int64     -- ^ the offset to set the pointer to
    -> DWORD     -- ^ the move method
    -> IO Int64  -- ^ the new absolute offset
setFilePointerEx hdl off meth = alloca $ \res -> do
    _ <- failIfZero
      "Network.Socket.SendFile.Win32.setFilePointerEx"
      (c_SetFilePointerEx hdl off res meth)
    peek res

sendfileI :: SOCKET -> HANDLE -> Int64 -> Int64 -> IO (Bool, Int64)
sendfileI out_fd in_hdl off count =
    do _ <- setFilePointerEx in_hdl off (#const FILE_BEGIN)
       transmitFile out_fd in_hdl (fromIntegral count)
       return (False, count)

transmitFile :: SOCKET -- ^ A handle to a connected socket.
             -> HANDLE -- ^ A handle to the open file that the TransmitFile function transmits.
             -> DWORD  -- ^ The number of bytes in the file to transmit.
             -> IO ()
transmitFile out_fd in_hdl count = do
    _ <- failIfZero
      "Network.Socket.SendFile.Win32.transmitFile"
      (c_TransmitFile out_fd in_hdl count 0 nullPtr nullPtr (#{const TF_USE_KERNEL_APC} .|. #{const TF_WRITE_BEHIND}))
    return ()
    -- according to msdn:
    --   If the TransmitFile function is called with the lpOverlapped parameter
    --   set to NULL, the operation is executed as synchronous I/O. The function
    --   will not complete until the file has been sent.

safeMinus :: (Ord a, Num a) => a -> a -> a
safeMinus x y
    | y >= x = 0
    | otherwise = x - y

-- max num of bytes in one send
-- Windows will complain of an "invalid argument" if you use the maxBound of a DWORD, despite the fact that the count parameter is a DWORD; so the upper bound of a 32-bit integer seems to be the real limit, similar to Linux.
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: Int32)
-- maxBytes = 32 * 1024

-- http://support.microsoft.com/kb/99173 - MAY BE IMPORTANT
-- http://msdn.microsoft.com/en-us/library/ks2530z6.aspx
foreign import ccall unsafe "io.h _get_osfhandle"
    c_get_osfhandle :: Fd
                    -> IO IntPtr

-- http://msdn.microsoft.com/en-us/library/aa365541(VS.85).aspx
foreign import stdcall unsafe "windows.h SetFilePointerEx" c_SetFilePointerEx
    :: HANDLE -> Int64 -> Ptr Int64 -> DWORD -> IO CInt

-- http://msdn.microsoft.com/en-us/library/ms740565(VS.85).aspx
foreign import stdcall safe "mswsock.h TransmitFile"
    c_TransmitFile :: SOCKET
                   -> HANDLE
                   -> DWORD
                   -> DWORD
                   -> Ptr ()
                   -> Ptr ()
                   -> DWORD
                   -> IO CInt
