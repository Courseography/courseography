module Models.Program
    (returnProgram,
    reqsForProgram) where

import Config (runDb)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import qualified Data.Text as T (Text, unpack)
import Database.Persist.Sqlite (entityVal, selectFirst, (==.))
import Database.Tables

-- | Queries the database for information about the program then returns the post value
returnProgram :: T.Text -> IO (Maybe Post)
returnProgram code = runDb $ do
    sqlProgram <- selectFirst [PostCode ==. code] []
    case sqlProgram of
        Nothing -> return Nothing
        Just program -> return $ Just $ entityVal program

-- | Retrieves the course requirements for a Post (program) as a list of course codes
reqsForProgram :: Post -> [String]
reqsForProgram program = do
    let requirementsText = T.unpack $ postRequirements program
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
