{-|
    Module      : Util.Happstack
    Description : Contains a single method for creating a JSON response.
-}
module Util.Happstack
    (createJSONResponse) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import Happstack.Server

-- | Creates a JSON response.
createJSONResponse :: ToJSON a => a -> Response
createJSONResponse x = toResponseBS (BS.pack "application/json") (encode x)
