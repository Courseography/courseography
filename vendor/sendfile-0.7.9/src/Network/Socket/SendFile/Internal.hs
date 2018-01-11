{-# LANGUAGE CPP, RecordWildCards #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFileIterWith,
    sendFile',
    sendFileIterWith',
    sendFile'',
    sendFileIterWith'',
    unsafeSendFile,
    unsafeSendFileIterWith,
    unsafeSendFile',
    unsafeSendFileIterWith',
    sendFileMode,
    ) where

#if defined(PORTABLE_SENDFILE)
import Network.Socket.SendFile.Portable (sendFileMode, sendFile'', sendFileIterWith'', unsafeSendFile'', unsafeSendFileIterWith'')
#else
import Network.Socket (fdSocket)
import Network.Socket.SendFile.Util
import System.Posix.Types (Fd(..))
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types (Handle__(..))
import qualified GHC.IO.FD as FD
-- import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)
#else
import GHC.IOBase
import GHC.Handle hiding (fdToHandle)
import qualified GHC.Handle
#endif
#endif
#endif

import Network.Socket (Socket)
import Network.Socket.SendFile.Iter (Iter(..))
import System.IO (Handle, IOMode(..), hFileSize, hFlush, withBinaryFile)
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import System.IO.Error
#endif
#endif 

#if defined(WIN32_SENDFILE)
import Network.Socket.SendFile.Win32 (_sendFile, sendFileIter)

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"
#endif

#if defined(LINUX_SENDFILE)
import Network.Socket.SendFile.Linux (_sendFile, sendFileIter)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
#endif

#if defined(FREEBSD_SENDFILE)
import Network.Socket.SendFile.FreeBSD (_sendFile, sendFileIter)

sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
#endif

#if defined(DARWIN_SENDFILE)
import Network.Socket.SendFile.Darwin (_sendFile, sendFileIter)

sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
#endif

#if defined(PORTABLE_SENDFILE)

#else
sendFile'' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile'' outs inh off count =
    do let out_fd = Fd (fdSocket outs)
       withFd inh $ \in_fd ->
         wrapSendFile' (\out_fd_ in_fd_ _blockSize_ off_ count_ -> _sendFile out_fd_ in_fd_ off_ count_)
                       out_fd in_fd count off count

sendFileIterWith'' :: (IO Iter -> IO a) -> Socket -> Handle -> Integer -> Integer -> Integer -> IO a
sendFileIterWith'' stepper outs inp blockSize off count =
    do let out_fd = Fd (fdSocket outs)
       withFd inp $ \in_fd ->
         stepper $ wrapSendFile' sendFileIter out_fd in_fd blockSize off count


unsafeSendFile'' :: Handle -> Handle -> Integer -> Integer -> IO ()
unsafeSendFile'' outp inp off count =
    do hFlush outp
       withFd outp $ \out_fd ->
         withFd inp $ \in_fd ->
          wrapSendFile' (\out_fd_ in_fd_ _blockSize_ off_ count_ -> _sendFile out_fd_ in_fd_ off_ count_)
                        out_fd in_fd count off count
--            wrapSendFile' _sendFile out_fd in_fd off count

unsafeSendFileIterWith'' :: (IO Iter -> IO a) -> Handle -> Handle -> Integer -> Integer -> Integer -> IO a
unsafeSendFileIterWith'' stepper outp inp blockSize off count =
    do hFlush outp
       withFd outp $ \out_fd ->
         withFd inp $ \in_fd ->
             stepper $ wrapSendFile' sendFileIter out_fd in_fd blockSize off count

-- The Fd should not be used after the action returns because the
-- Handler may be garbage collected and than will cause the finalizer
-- to close the fd.
withFd :: Handle -> (Fd -> IO a) -> IO a
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
withFd h f = withHandle_ "withFd" h $ \ Handle__{..} -> do
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "withFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> f (Fd (fromIntegral (FD.fdFD fd)))
#else
withFd h f =
    withHandle_ "withFd" h $ \ h_ ->
      f (Fd (fromIntegral (haFD h_)))
#endif
#endif


#endif

sendFile :: Socket -> FilePath -> IO ()
sendFile outs infp =
    withBinaryFile infp ReadMode $ \inp -> do
      count <- hFileSize inp
      sendFile'' outs inp 0 count

sendFileIterWith :: (IO Iter -> IO a) -> Socket -> FilePath -> Integer -> IO a
sendFileIterWith stepper outs infp blockSize =
    withBinaryFile infp ReadMode $ \inp -> do
      count <- hFileSize inp
      sendFileIterWith'' stepper outs inp blockSize 0 count

sendFile' :: Socket -> FilePath -> Integer -> Integer -> IO ()
sendFile' outs infp offset count =
    withBinaryFile infp ReadMode $ \inp ->
        sendFile'' outs inp offset count

sendFileIterWith' :: (IO Iter -> IO a) -> Socket -> FilePath -> Integer -> Integer -> Integer -> IO a
sendFileIterWith' stepper outs infp blockSize offset count =
    withBinaryFile infp ReadMode $ \inp ->
        sendFileIterWith'' stepper outs inp blockSize offset count

unsafeSendFile :: Handle -> FilePath -> IO ()
unsafeSendFile outp infp =
    withBinaryFile infp ReadMode $ \inp -> do
      count <- hFileSize inp
      unsafeSendFile'' outp inp 0 count

unsafeSendFileIterWith :: (IO Iter -> IO a) -> Handle -> FilePath -> Integer -> IO a
unsafeSendFileIterWith stepper outp infp blockSize =
    withBinaryFile infp ReadMode $ \inp -> do
      count <- hFileSize inp
      unsafeSendFileIterWith'' stepper outp inp blockSize 0 count


unsafeSendFile'
    :: Handle    -- ^ The output handle
    -> FilePath  -- ^ The input filepath
    -> Integer    -- ^ The offset to start at
    -> Integer -- ^ The number of bytes to send
    -> IO ()
unsafeSendFile' outp infp offset count =
    withBinaryFile infp ReadMode $ \inp -> do
      unsafeSendFile'' outp inp offset count

unsafeSendFileIterWith'
    :: (IO Iter -> IO a)
    -> Handle    -- ^ The output handle
    -> FilePath  -- ^ The input filepath
    -> Integer   -- ^ maximum block size
    -> Integer   -- ^ The offset to start at
    -> Integer   -- ^ The number of bytes to send
    -> IO a
unsafeSendFileIterWith' stepper outp infp blockSize offset count =
    withBinaryFile infp ReadMode $ \inp -> do
      unsafeSendFileIterWith'' stepper outp inp blockSize offset count
