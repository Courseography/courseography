module Models.Requirement (
    parseReqs,
) where

import Data.Char
import qualified Data.Text as T
import Database.Requirement
import qualified Text.Parsec as Parsec
import WebParsing.ReqParser

-- | Parses prerequisite strings into the Requirement datatype
parseReqs :: T.Text -> Req
parseReqs reqText =
    let reqString = T.unpack reqText
        reqStringLower = map toLower reqString
     in if all isSpace reqString || reqStringLower == "none" || reqStringLower == "no"
            then None
            else do
                let req = Parsec.parse reqParser "" reqString
                 in case req of
                        Right x -> x
                        Left e -> J (show e) ""
