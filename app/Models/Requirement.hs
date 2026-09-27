module Models.Requirement (
    parseReqs,
) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Database.Requirement (Req (J, None))
import qualified Text.Parsec as Parsec
import WebParsing.ReqParser (reqParser)

-- | Parses prerequisite strings into the Requirement datatype
parseReqs :: T.Text -> Req
parseReqs reqText =
    let reqLower = T.toLower reqText
        reqString = T.unpack reqText
     in if all isSpace reqString || reqLower == "none" || reqLower == "no"
            then None
            else do
                let req = Parsec.parse reqParser "" reqString
                 in case req of
                        Right x -> x
                        Left e -> J (show e) ""
