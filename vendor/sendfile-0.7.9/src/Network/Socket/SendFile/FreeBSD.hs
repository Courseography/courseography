{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile, sendFileIter, sendfile) where
import Data.Int (Int64)
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import System.Posix.Types (COff(..), Fd(..))

-- | automatically loop and send everything
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
    sendFileIterI out_fd in_fd (min (fromIntegral blockSize) maxBytes) (fromIntegral off) (fromIntegral count)

sendFileIterI :: Fd -- ^ file descriptor corresponding to network socket
              -> Fd -- ^ file descriptor corresponding to file
              -> CSize -- ^ maximum number of bytes to send at once
              -> COff -- ^ offset into file
              -> CSize -- ^ total number of bytes to send
              -> IO Iter
sendFileIterI _out_fd _in_fd _blockSize _off  0         = return (Done 0)
sendFileIterI  out_fd  in_fd  blockSize  off  remaining =
    do let bytes = min remaining blockSize
       (wouldBlock, nsent) <- alloca $ \sbytes -> sendfileI out_fd in_fd off bytes sbytes
       let cont = sendFileIterI out_fd in_fd blockSize (off + nsent) (remaining `safeMinus` (fromIntegral nsent))
       case wouldBlock of
         True  -> return (WouldBlock (fromIntegral nsent) out_fd cont)
         False -> return (Sent (fromIntegral nsent) cont)

-- | low-level wrapper around sendfile
-- non-blocking
-- returns number of bytes written and if EAGAIN
-- does not call 'threadWaitWrite'
sendfile :: Fd -> Fd -> Int64 -> Int64 -> IO (Bool, Int64)
sendfile out_fd in_fd off count =
    alloca $ \sbytes ->
        do (wb, sent) <- sendfileI out_fd in_fd (fromIntegral off) (fromIntegral count) sbytes
           return (wb, fromIntegral sent)

-- NOTE: should we retry automatically on EINTR (but not EAGAIN)
sendfileI :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO (Bool, COff)
sendfileI out_fd in_fd off count sbytes =
    do status <- c_sendfile out_fd in_fd off count sbytes
       if (status == 0)
          then do nsent <- peek sbytes
                  return (False, nsent)
          else do errno <- getErrno
                  if (errno == eAGAIN) || (errno == eINTR)
                   then do nsent <- peek sbytes
                           return (True, nsent)
                   else throwErrno "Network.Socket.SendFile.FreeBSD.sendfileI"

safeMinus :: (Ord a, Num a) => a -> a -> a
safeMinus x y
    | y >= x = 0
    | otherwise = x - y

-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt

c_sendfile :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO CInt
c_sendfile out_fd in_fd off count sbytes = c_sendfile_freebsd in_fd out_fd off count nullPtr sbytes 0
