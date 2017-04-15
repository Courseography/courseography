{-|
    Module      : Util.Happstack
    Description : Contains a single method for creating a JSON response.
-}
module Util.Happstack
    (createJSONResponse) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Happstack.Server
import Data.Aeson (ToJSON, encode)

-- | Creates a JSON response.
createJSONResponse :: ToJSON a => a -> Response
createJSONResponse x = toResponseBS (BS.pack "application/json") (encodeJSON x)
    where
        encodeJSON :: ToJSON a => a -> BSL.ByteString
        encodeJSON = BSL.filter (/= '\\') . encode
