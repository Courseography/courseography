module JsonResponse where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Happstack.Server
import Data.Aeson as Aeson
import Database.JsonParser (encodeJSON)

-- | Creates a JSON response.
createJSONResponse :: ToJSON a => a -> Response
createJSONResponse x = toResponseBS (BS.pack "application/json") $ encodeJSON $ Aeson.toJSON x