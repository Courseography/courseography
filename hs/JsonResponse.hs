module JsonResponse where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Happstack.Server

-- | Creates a JSON response.
createJSONResponse :: BSL.ByteString -> Response
createJSONResponse jsonStr = toResponseBS (BS.pack "application/json") jsonStr