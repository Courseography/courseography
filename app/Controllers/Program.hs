module Controllers.Program(index) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T (Text, unlines)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, entityVal, selectList)
import Database.Tables as Tables (Post, postCode)
import Happstack.Server (Response, ServerPart, toResponse)

-- | Builds a list of all program codes in the database
index :: ServerPart Response
index = do
    response <- liftIO $ runDb $ do
        programsList :: [Entity Post] <- selectList [] []
        let codes = map (postCode . entityVal) programsList
        return $ T.unlines codes :: SqlPersistM T.Text
    return $ toResponse response
