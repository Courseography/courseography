module Controllers.Post (retrievePost) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T (Text)
import Database.Tables
import Happstack.Server.SimpleHTTP (Request, Response, ServerPart, askRq, ifModifiedSince,
                                    lookText')
import Models.Post (returnPost)
import Util.Happstack (createJSONResponse)

-- | Takes a http request with a post code and sends a JSON response containing the post data
-- | if the post data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
retrievePost :: ServerPart Response
retrievePost = do
    req <- askRq
    code <- lookText' "code"
    liftIO $ queryPost req code

-- | Queries the database for the post data then returns a JSON response of it
-- | if the post data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
queryPost :: Request -> T.Text -> IO Response
queryPost req code = do
    postMaybe <- returnPost code
    case postMaybe of
        Nothing -> return $ createJSONResponse (Nothing :: Maybe Post)
        Just post -> return $ ifModifiedSince (postModified post) req (createJSONResponse post)
