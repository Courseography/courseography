module Network.Socket.SendFile.Util
    ( wrapSendFile'
    ) where

-- | wraps sendFile' to check arguments
wrapSendFile' :: Integral i => (a -> b -> i -> i -> i -> IO c) -> a -> b -> Integer -> Integer -> Integer -> IO c
wrapSendFile' fun outp inp blockSize off count
--    | count     == 0 = return () -- Send nothing -- why do the work? Also, Windows and FreeBSD treat '0' as 'send the whole file'.
    | count     <  0 = error "SendFile - count must be a positive integer"
    | (count /= 0) && (blockSize <= 0) = error "SendFile - blockSize must be a positive integer greater than 1"
    | off       <  0 = error "SendFile - offset must be a positive integer"
    | otherwise      = fun outp inp (fromIntegral blockSize) (fromIntegral off) (fromIntegral count)
