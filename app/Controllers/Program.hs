module Controllers.Program(index, retrieveProgram) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import qualified Data.Text as T (Text, null, strip, unlines)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, entityVal, selectList)
import Database.Tables as Tables (Post, postCode, postModified)
import Happstack.Server (Request, Response, ServerPart, askRq, ifModifiedSince, lookText',
                         toResponse)
import Models.Program (returnProgram)
import Util.Happstack (createJSONResponse)

-- | Builds a list of all program codes in the database
index :: ServerPart Response
index = do
    response <- liftIO $ runDb $ do
        programsList :: [Entity Post] <- selectList [] []
        let codes = map (postCode . entityVal) programsList
            rmEmpty = filter (not . T.null . T.strip) codes
            rmDups = S.toList (S.fromList rmEmpty)
        return $ T.unlines rmDups :: SqlPersistM T.Text
    return $ toResponse response

-- | Takes a http request with a program code and sends a JSON response containing the program data
-- | if the program data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
retrieveProgram :: ServerPart Response
retrieveProgram = do
    req <- askRq
    code <- lookText' "code"
    liftIO $ queryProgram req code

-- | Queries the database for the program data then returns a JSON response of it
-- | if the program data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
queryProgram :: Request -> T.Text -> IO Response
queryProgram req code = do
    programMaybe <- returnProgram code
    case programMaybe of
        Nothing -> return $ createJSONResponse (Nothing :: Maybe Post)
        Just program -> return $ ifModifiedSince (postModified program) req (createJSONResponse program)
