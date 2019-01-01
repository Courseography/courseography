{-# LANGUAGE ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Darwin (_sendFile, sendFileIter, sendfile) where

import Data.Int (Int64)
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import System.Posix.Types (Fd(..))
#include <stdio.h>

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count =
    do _ <- runIter (sendFileIter out_fd in_fd count off count) -- set blockSize == count. ie. send it all if we can.
       return ()

-- | a way to send things in chunks
sendFileIter :: Fd -- ^ file descriptor corresponding to network socket
             -> Fd -- ^ file descriptor corresponding to file
             -> Int64 -- ^ maximum number of bytes to send at once
             -> Int64 -- ^ offset into file
             -> Int64 -- ^ total number of bytes to send
             -> IO Iter
sendFileIter out_fd in_fd blockSize off remaining =
        sendFileIterI out_fd in_fd (min blockSize maxBytes) off remaining

sendFileIterI :: Fd -- ^ file descriptor corresponding to network socket
              -> Fd -- ^ file descriptor corresponding to file
              -> Int64 -- ^ maximum number of bytes to send at once
              -> Int64 -- ^ offset into file
              -> Int64 -- ^ total number of bytes to send
--              -> Ptr Int64 -- ^ sent bytes ptr
              -> IO Iter
sendFileIterI _out_fd _in_fd _blockSize _off  0         = return (Done 0)
sendFileIterI  out_fd  in_fd  blockSize  off  remaining =
    do let bytes = min remaining blockSize
       (wouldBlock, nsent) <- alloca $ \len ->
                                do poke len bytes
                                   sendfileI out_fd in_fd off len
       let cont = sendFileIterI out_fd in_fd blockSize (off + nsent) (remaining `safeMinus` nsent)
       case wouldBlock of
         True  -> return (WouldBlock (fromIntegral nsent) out_fd cont)
         False -> return (Sent (fromIntegral nsent) cont)


-- | low-level wrapper around sendfile
-- non-blocking
-- returns number of bytes written and if EAGAIN/EINTR
-- does not call 'threadWaitWrite'
sendfile :: Fd -> Fd -> Int64 -> Int64 -> IO (Bool, Int64)
sendfile out_fd in_fd off count =
    alloca $ \len ->
        do poke len count
           sendfileI out_fd in_fd off len

-- NOTE: should we retry automatically on EINTR (but not EAGAIN)
sendfileI :: Fd -> Fd -> Int64 -> Ptr Int64 -> IO (Bool, Int64)
sendfileI out_fd in_fd off len =
    do status <- c_sendfile out_fd in_fd off len
       if (status == 0)
          then do nsent <- peek len
                  return $ (False, nsent)
          else do errno <- getErrno
                  if (errno == eAGAIN) || (errno == eINTR)
                   then do nsent <- peek len
                           return (True, nsent)
                   else throwErrno "Network.Socket.SendFile.Darwin.sendfileI"

safeMinus :: (Ord a, Num a) => a -> a -> a
safeMinus x y
    | y >= x = 0
    | otherwise = x - y

-- max num of bytes in one send
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: (#type off_t))

-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> Ptr () -> CInt -> IO CInt

c_sendfile :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO CInt
c_sendfile out_fd in_fd off pbytes = c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0
