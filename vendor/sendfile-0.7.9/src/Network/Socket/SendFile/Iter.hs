module Network.Socket.SendFile.Iter where

import Control.Concurrent (threadWaitWrite)
import Data.Int           (Int64)
import System.Posix.Types (Fd)

-- | An iteratee for sendfile
--
-- In general, a whole file is not sent by a single call to
-- sendfile(), but a series of calls which send successive pieces.
--
-- The high-level API in this sendfile library has calls which will
-- send the entire file (or an entire requested offset+length), before
-- returning.
--
-- However, there are instances where you want to be a bit more
-- involved in the sending loop. For example, if you want to tickle a
-- timeout after each chunk is sent or update a progress bar.
--
-- The 'Iter' type gives you that power with out requiring you to
-- manage all the low-level details of the sendfile loop. The
-- interface is simple and consistant across all platforms.
--
-- A call to sendfile() can result in three different states:
--
--  (1) the requested number of bytes for that iteration was sent
--  successfully, there are more bytes left to send.
--
--  (2) some (possibly 0) bytes were sent, but the file descriptor
--  would now block if more bytes were written. There are more bytes
--  left to send.
--
--  (2) All the bytes were sent, and there is nothing left to send.
--
-- We handle these three cases by using a type with three
-- constructors:
-- 
-- @
--  data Iter
--      = Sent       Int64    (IO Iter)
--      | WouldBlock Int64 Fd (IO Iter)
--      | Done       Int64             
-- @
--
-- All three constructors provide an 'Int64' which represents the
-- number of bytes sent for that particular iteration. (Not the total
-- byte count).
--
-- The 'Sent' and 'WouldBlock' constructors provide 'IO' 'Iter' as their
-- final argument. Running this IO action will send the next block of
-- data.
--
-- The 'WouldBlock' constructor also provides the 'Fd' for the output
-- socket. You should not send anymore data until the 'Fd' would not
-- block. The easiest way to do that is to use 'threadWaitWrite' to
-- suspend the thread until the 'Fd' is available.
--
-- A very simple function to drive the Iter might look like:
-- 
-- @
-- runIter :: IO Iter -> IO ()
-- runIter iter =
--    do r <- iter
--       case r of
--         (Done _n)      -> return ()
--         (Sent _n cont) -> runIter cont
--         (WouldBlock _n fd cont) -> 
--             do threadWaitWrite fd
--                runIter cont
-- @
--
-- You would use it as the first argument to a *IterWith function, e.g.
--
-- @
--  sendFileIterWith runIter outputSocket \"\/path\/to\/file\" 2^16 
-- @
--
-- The 'runIter' function provided by this module is similar, but also returns the total number of bytes sent.
--
-- NOTE: You must not use the 'Fd' or the 'IO' 'Iter' after the call
-- to *IterWith has returned. When the *IterWith functions return,
-- the file descriptors may be closed due to finalizers running.
data Iter
    = Sent       Int64    (IO Iter) -- ^ number of bytes sent this pass and a continuation to send more
    | WouldBlock Int64 Fd (IO Iter) -- ^ number of bytes sent, Fd that blocked, continuation to send more. NOTE: The Fd should not be used outside the running of the Iter as it may be freed when the Iter is done
    | Done       Int64              -- ^ number of bytes sent, no more to send

-- | A simple function to drive the *IterWith functions.
-- It returns the total number of bytes sent.
runIter :: IO Iter -> IO Int64
runIter = runIter' 0
    where
      runIter' :: Int64 -> IO Iter -> IO Int64
      runIter' acc iter =
          do r <- iter
             case r of
               (Sent n cont) -> 
                   do let acc' = (acc + n) 
--                      putStrLn $ "Sent " ++ show acc'
                      acc' `seq` runIter' acc' cont
               (Done n) ->
                   do -- putStrLn $ "Done " ++ show (acc + n)
                      return (acc + n)
               (WouldBlock n fd cont) -> 
                   do threadWaitWrite fd
                      let acc' = (acc + n) 
--                      putStrLn $ "WouldBlock " ++ (show acc')
                      acc' `seq` runIter' acc' cont
