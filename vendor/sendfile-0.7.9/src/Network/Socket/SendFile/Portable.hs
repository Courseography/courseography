{-# LANGUAGE CPP #-}
module Network.Socket.SendFile.Portable 
     ( sendFile
     , sendFileIterWith
     , sendFile'
     , sendFileIterWith'
     , sendFile''
     , sendFileIterWith''
     , unsafeSendFile
     , unsafeSendFileIterWith
     , unsafeSendFile'
     , unsafeSendFile''
     , unsafeSendFileIterWith'
     , unsafeSendFileIterWith''
     , sendFileMode
     )
    where

import Data.ByteString.Char8 (hGet, hPut, length, ByteString)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (send)
import Network.Socket (Socket(..), fdSocket)
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import Network.Socket.SendFile.Util (wrapSendFile')
import Prelude hiding (length)
import System.IO (Handle, IOMode(..), SeekMode(..), hFileSize, hFlush, hIsEOF, hSeek, withBinaryFile)
import System.Posix.Types (Fd(..))
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import System.IO.Error
#endif
#endif 


sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"

sendFileIterWith'' :: (IO Iter -> IO a) -> Socket -> Handle -> Integer -> Integer -> Integer -> IO a
sendFileIterWith'' stepper =
    wrapSendFile' $ \outs inp blockSize off count ->
        do hSeek inp AbsoluteSeek off
           stepper (sendFileIterS outs inp blockSize {- off -} count Nothing)

sendFile'' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile'' outs inh off count =
    do _ <- sendFileIterWith'' runIter outs inh count off count
       return ()

unsafeSendFileIterWith'' :: (IO Iter -> IO a) -> Handle -> Handle -> Integer -> Integer -> Integer -> IO a
unsafeSendFileIterWith'' stepper =
    wrapSendFile' $ \outp inp blockSize off count ->
        do hSeek inp AbsoluteSeek off
           a <- stepper (unsafeSendFileIter outp inp blockSize count Nothing)
           hFlush outp
           return a

unsafeSendFile'' :: Handle -> Handle -> Integer -> Integer -> IO ()
unsafeSendFile'' outh inh off count =
    do _ <- unsafeSendFileIterWith'' runIter outh inh count off count
       return ()

sendFileIterS :: Socket  -- ^ output network socket
             -> Handle  -- ^ input handle
             -> Integer -- ^ maximum number of bytes to send at once
             -> Integer -- ^ total number of bytes to send
             -> Maybe ByteString
             -> IO Iter
sendFileIterS _socket _inh _blockSize {- _off -} 0        _    = return (Done 0)
sendFileIterS socket   inh  blockSize {- off -} remaining mBuf =
    do buf <- nextBlock
       nsent <- send socket buf
       let leftOver =
               if nsent < (C.length buf)
                  then Just (C.drop nsent buf)
                  else Nothing
       let cont = sendFileIterS socket inh blockSize {- (off + (fromIntegral nsent)) -} (remaining `safeMinus` (fromIntegral nsent)) leftOver
       if nsent < (length buf)
          then return (WouldBlock (fromIntegral nsent) (Fd $ fdSocket socket) cont)
          else return (Sent       (fromIntegral nsent)                        cont)
    where
   nextBlock =
          case mBuf of
            (Just b) -> return b
            Nothing ->
                do eof <- hIsEOF inh
                   if eof
                    then ioError (mkIOError eofErrorType ("Reached EOF but was hoping to read " ++ show remaining ++ " more byte(s).") (Just inh) Nothing)
                    else do let bytes = min 32768 (min blockSize remaining)
                            hGet inh (fromIntegral bytes) -- we could check that we got fewer bytes than requested here, but we will send what we got and catch the EOF next time around

safeMinus :: (Show a, Ord a, Num a) => a -> a -> a
safeMinus x y
    | y > x = error $ "y > x " ++ show (y,x)
    | otherwise = x - y

unsafeSendFileIter :: Handle  -- ^ output handle
                   -> Handle  -- ^ input handle
                   -> Integer -- ^ maximum number of bytes to send at once
--                   -> Integer -- ^ offset into file
                   -> Integer -- ^ total number of bytes to send
                   -> Maybe ByteString
                   -> IO Iter
unsafeSendFileIter _outh _inh _blockSize 0         _mBuf = return (Done 0)
unsafeSendFileIter  outh  inh  blockSize remaining  mBuf =
    do buf <- nextBlock
       hPut outh buf -- eventually this should use a non-blocking version of hPut
       let nsent = length buf
{-
           leftOver =
               if nsent < (C.length buf)
                  then Just (C.drop nsent buf)
                  else Nothing
-}
           cont = unsafeSendFileIter outh inh blockSize {- (off + (fromIntegral nsent)) -} (remaining - (fromIntegral nsent)) Nothing
       if nsent < (length buf)
          then do error "unsafeSendFileIter: internal error" -- return (WouldBlock (fromIntegral nsent) (Fd $ fdSocket socket) cont)
          else return (Sent (fromIntegral nsent) cont)
    where
      nextBlock =
          case mBuf of
            (Just b) -> return b
            Nothing ->
                do eof <- hIsEOF inh
                   if eof
                    then ioError (mkIOError eofErrorType ("Reached EOF but was hoping to read " ++ show remaining ++ " more byte(s).") (Just inh) Nothing)
                    else do let bytes = min 32768 (min blockSize remaining)
                            hGet inh (fromIntegral bytes) -- we could check that we got fewer bytes than requested here, but we will send what we got and catch the EOF next time around

-- copied from Internal.hs -- not sure how to avoid having two copies of this code yet

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
