-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation.
--
--   Two interfaces are provided for both the unsafe and safe sets of functions. The first interface accepts an output socket\/handle and the path of the file you want to send; sendFile and unsafeSendFile comprise this interface. The second interface accepts an output socket\/handle, a handle to the file you want to send, an offset, and the number of bytes you want to send; sendFile' and unsafeSendFile' comprise this interface.
--
--   For consistent read/write behavior with either sendFile' or unsafeSendFile', the input handle should be opened in Binary mode rather than Text mode.
--

module Network.Socket.SendFile (
    ByteCount,
    Offset,
    Iter(..), runIter,
    -- * Safe functions (recommended)
    sendFile,
    sendFileIterWith,
    sendFile',
    sendFileIterWith',
    -- * Unsafe functions
    -- | These functions are unsafe simply because there is no guarantee that the 'Handle' used for output is actually bound to a 'Socket'. If it is not, it will result in a runtime error.
    unsafeSendFile,
    unsafeSendFileIterWith,
    unsafeSendFile',
    unsafeSendFileIterWith',
    -- * Utility functions
    sendFileMode
    ) where
import qualified Network.Socket.SendFile.Internal (sendFile, sendFileIterWith, sendFile', sendFileIterWith', 
                                                   unsafeSendFile, unsafeSendFileIterWith, unsafeSendFile', unsafeSendFileIterWith', 
                                                   sendFileMode)
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import Network.Socket (Socket)
import System.IO (Handle)

-- | The file offset (in bytes) to start from
type Offset = Integer

-- | The length (in bytes) which should be sent
type ByteCount = Integer

-- | The simplest interface. Simply give it an output `Socket` and the `FilePath` to the input file.
sendFile
    :: Socket   -- ^ The output socket
    -> FilePath -- ^ The path where the input file resides
    -> IO ()
sendFile = Network.Socket.SendFile.Internal.sendFile

-- | The simplest interface. Simply give it an output `Socket` and the `FilePath` to the input file.
--
-- This variant takes a function to drive the iteration loop. See 'Iter' for more information.
sendFileIterWith
    :: (IO Iter -> IO a)
    -> Socket    -- ^ The output socket
    -> FilePath  -- ^ The path where the input file resides
    -> ByteCount -- ^ Maximum bytes to send per block (may send less)
    -> IO a
sendFileIterWith = Network.Socket.SendFile.Internal.sendFileIterWith

-- | A more powerful interface than sendFile which accepts a starting offset, and the bytecount to send; the offset and the count must be a positive integer. The initial position of the input file handle matters not since the offset is absolute, and the final position may be different depending on the platform -- no assumptions can be made.
sendFile'
    :: Socket    -- ^ The output socket
    -> FilePath    -- ^ The input file path
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO ()
sendFile' = Network.Socket.SendFile.Internal.sendFile'

-- | A more powerful interface than sendFile which accepts a starting offset, and the bytecount to send; the offset and the count must be a positive integer. The initial position of the input file handle matters not since the offset is absolute, and the final position may be different depending on the platform -- no assumptions can be made.
--
-- This variant takes a function to drive the iteration loop. See 'Iter' for more information.
sendFileIterWith'
    :: (IO Iter -> IO a)
    -> Socket    -- ^ The output socket
    -> FilePath    -- ^ The input file path
    -> ByteCount -- ^ Maximum bytes to send per block (may send less)
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO a
sendFileIterWith' = Network.Socket.SendFile.Internal.sendFileIterWith'

-- | The unsafe version of sendFile which accepts a `Handle` instead of a `Socket` for the output.  It will flush the output handle before sending any file data.
unsafeSendFile
    :: Handle   -- ^ The output handle
    -> FilePath -- ^ The path where the input file resides
    -> IO ()
unsafeSendFile = Network.Socket.SendFile.Internal.unsafeSendFile

-- | The unsafe version of sendFile which accepts a `Handle` instead of a `Socket` for the output.  It will flush the output handle before sending any file data.
--
-- This variant takes a function to drive the iteration loop. See 'Iter' for more information.
unsafeSendFileIterWith
    :: (IO Iter -> IO a)
    -> Handle    -- ^ The output handle
    -> FilePath  -- ^ The path where the input file resides
    -> ByteCount -- ^ Maximum bytes to send per block (may send less)
    -> IO a
unsafeSendFileIterWith = Network.Socket.SendFile.Internal.unsafeSendFileIterWith

-- | The unsafe version of sendFile' which accepts a `Handle` instead of a `Socket` for the output. It will flush the output handle before sending any file data.
unsafeSendFile'
    :: Handle    -- ^ The output handle
    -> FilePath  -- ^ The input filepath
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO ()
unsafeSendFile' = Network.Socket.SendFile.Internal.unsafeSendFile'

-- | The unsafe version of sendFile' which accepts a `Handle` instead of a `Socket` for the output. It will flush the output handle before sending any file data.
--
-- This variant takes a function to drive the iteration loop. See 'Iter' for more information.
unsafeSendFileIterWith'
    :: (IO Iter -> IO a)
    -> Handle    -- ^ The output handle
    -> FilePath  -- ^ The input filepath
    -> ByteCount -- ^ The number of bytes to send
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO a
unsafeSendFileIterWith' = Network.Socket.SendFile.Internal.unsafeSendFileIterWith'

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use.
-- Possible values are 'WIN32_SENDFILE', 'LINUX_SENDFILE', 'FREEBSD_SENDFILE',
-- 'DARWIN_SENDFILE', and 'PORTABLE_SENDFILE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = Network.Socket.SendFile.Internal.sendFileMode

