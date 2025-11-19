module Models.Program
    (returnProgram,
    reqsForProgram) where

import Config (runDb)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import qualified Data.Text as T (Text, unpack)
import Database.Persist.Sqlite (entityVal, selectFirst, (==.))
import Database.Tables

-- | Queries the database for information about the program then returns the program value
returnProgram :: T.Text -> IO (Maybe Program)
returnProgram code = runDb $ do
    sqlProgram <- selectFirst [ProgramCode ==. code] []
    case sqlProgram of
        Nothing -> return Nothing
        Just program -> return $ Just $ entityVal program

-- | Retrieves the course requirements for a Program as a list of course codes
reqsForProgram :: Program -> [String]
reqsForProgram program = do
    let requirementsText = T.unpack $ programRequirements program
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
