module Models.Post
    (retrievePost,
    returnPost,
    reqsForPost) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import qualified Data.Text as T (Text, unpack)
import Database.Persist.Sqlite (entityVal, selectFirst, (==.))
import Database.Tables
import Happstack.Server.SimpleHTTP (Request, Response, ServerPart, askRq, ifModifiedSince,
                                    lookText')
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

-- | Queries the database for information about the post then returns the post value
returnPost :: T.Text -> IO (Maybe Post)
returnPost code = runDb $ do
    sqlPost <- selectFirst [PostCode ==. code] []
    case sqlPost of
        Nothing -> return Nothing
        Just post -> return $ Just $ entityVal post

-- | Retrieves the course requirements for a Post as a list of course codes
reqsForPost :: Post -> [String]
reqsForPost post = do
    let requirementsText = T.unpack $ postRequirements post
        cleaned = filter (`notElem` ("<>" :: String)) $ filter (not . isPunctuation) requirementsText
        potentialCodes = words cleaned
    filter isCourseCode potentialCodes
  where
    -- | TODO: change function to use a regex
    isCourseCode :: String -> Bool
    isCourseCode codeStr =
        length codeStr == 8 &&
        all isAlphaNum codeStr &&
        all isAlpha (take 3 codeStr) &&
        all isDigit (take 3 (drop 3 codeStr)) &&
        isAlpha (codeStr !! 6) &&
        isDigit (codeStr !! 7)
