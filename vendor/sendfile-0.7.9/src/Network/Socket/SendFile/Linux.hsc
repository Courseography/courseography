{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (_sendFile, sendFileIter, sendfile) where

import Data.Int (Int32, Int64)    -- Int64 is imported on 64-bit systems
import Data.Word (Word32, Word64) -- Word64 is imported on 64-bit systems
import Foreign.C (CInt(..))
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable(poke)
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import System.Posix.Types (Fd(..))
#include <sys/sendfile.h>
#include <stdio.h>

-- | automatically loop and send everything
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
--    alloca $ \poff -> 
--        do poke poff off
           sendFileIterI out_fd in_fd (min blockSize maxBytes) off remaining

sendFileIterI :: Fd -- ^ file descriptor corresponding to network socket
              -> Fd -- ^ file descriptor corresponding to file
              -> Int64 -- ^ maximum number of bytes to send at once
              -> Int64 -- ^ offset into file
              -> Int64     -- ^ total number of bytes to send
              -> IO Iter
sendFileIterI _out_fd _in_fd _blockSize _off  0         = return (Done 0)
sendFileIterI  out_fd  in_fd  blockSize  off  remaining =
    do let bytes = min remaining blockSize
       (wouldBlock, sbytes) <- sendfile out_fd in_fd off bytes
       let cont = sendFileIterI out_fd in_fd blockSize (off + sbytes) (remaining `safeMinus` sbytes)
       case wouldBlock of
         True  -> return (WouldBlock sbytes out_fd cont)
         False -> return (Sent sbytes cont)

-- | low-level wrapper around sendfile
-- non-blocking
-- returns number of bytes written and whether the fd would block (aka, EAGAIN)
-- does not call 'threadWaitWrite'
sendfile :: Fd -> Fd -> Int64 -> Int64 -> IO (Bool, Int64)
sendfile out_fd in_fd off bytes = 
    alloca $ \poff -> 
        do poke poff off
           sendfileI out_fd in_fd poff bytes

-- low-level wrapper around linux sendfile
sendfileI :: Fd -> Fd -> Ptr Int64 -> Int64 -> IO (Bool, Int64)
sendfileI out_fd in_fd poff bytes = do
    sbytes <- {-# SCC "c_sendfile" #-} c_sendfile out_fd in_fd poff (fromIntegral bytes)
    if sbytes <= -1
      then do errno <- getErrno
              if errno == eAGAIN
                then return (True, 0)
                else throwErrno "Network.Socket.SendFile.Linux.sendfileI"
      else return (False, fromIntegral sbytes)

safeMinus :: (Ord a, Num a, Show a) => a -> a -> a
safeMinus x y
    | y > x = error $ "y > x " ++ show (y,x)
    | otherwise = x - y


-- max num of bytes in one send
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: (#type ssize_t))

-- sendfile64 gives LFS support
foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (#type off64_t) -> (#type size_t) -> IO (#type ssize_t)

