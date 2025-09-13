module Models.Post
    (returnPost,
    reqsForPost) where

import Config (runDb)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import qualified Data.Text as T (Text, unpack)
import Database.Persist.Sqlite (entityVal, selectFirst, (==.))
import Database.Tables

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
