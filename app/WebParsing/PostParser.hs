module WebParsing.PostParser
    (addPostToDatabase) where

import Control.Monad.Trans (liftIO)
import Data.Either (fromRight)
import Data.List (find)
import Data.List.Split (keepDelimsL, split, splitWhen, whenElt)
import Data.Text (strip)
import qualified Data.Text as T
import Database.DataType (PostType (..))
import Database.Persist (insertUnique)
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import WebParsing.ParsecCombinators (parseUntil, text)
import WebParsing.ReqParser (parseReqs)


addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    let fullPostName = maybe "" (strip . fromTagText) $ find isTagText programElements
        postHtml = sections isRequirementSection programElements
        requirementLines = if null postHtml then [] else reqHtmlToLines $ last postHtml
        requirements = concatMap parseRequirement requirementLines
    liftIO $ print fullPostName

    case P.parse postInfoParser "POSt information" fullPostName of
        Left _ -> return ()
        Right post -> do
            postExists <- insertUnique post
            case postExists of
                Just key ->
                    mapM_ (insert_ . PostCategory key) requirements
                Nothing -> return ()
    where
        isRequirementSection tag = or [tagOpenAttrNameLit "div" "class" (T.isInfixOf "views-field-field-enrolment-requirements") tag, tagOpenAttrNameLit "div" "class" (T.isInfixOf "views-field-field-completion-requirements") tag]


-- | Parse a Post value from its title.
-- Titles are usually of the form "Actuarial Science Major (Science Program)".
postInfoParser :: Parser Post
postInfoParser = do
    deptName <- parseDepartmentName
    postType <- parsePostType P.<|> return Other
    return $ Post postType deptName "" ""

    where
        parseDepartmentName :: Parser T.Text
        parseDepartmentName = parseUntil $ P.choice [
            P.lookAhead parsePostType >> return (),
            P.char '(' >> return ()
            ]

        parsePostType :: Parser PostType
        parsePostType = do
            postTypeName <- P.choice $ map (P.try . text) ["Specialist", "Major", "Minor"]
            return $ read $ T.unpack postTypeName


-- | Split requirements HTML into individual lines.
reqHtmlToLines :: [Tag T.Text] -> [[T.Text]]
reqHtmlToLines tags =
    let sects = split (keepDelimsL $ whenElt isSectionSplit) tags
        sectionsNoNotes = filter (not . isNoteSection) sects
        paragraphs = concatMap (splitWhen (isTagOpenName "p")) sectionsNoNotes
        lines' = map (map (T.strip . convertLine) . splitLines) paragraphs
    in
        lines'

    where
        isSectionSplit :: Tag T.Text -> Bool
        isSectionSplit tag =
            isTagText tag &&
            any (flip T.isInfixOf $ fromTagText tag) ["First", "Second", "Third", "Higher", "Notes", "NOTES"]

        isNoteSection :: [Tag T.Text] -> Bool
        isNoteSection (sectionTitleTag:_) =
            isTagText sectionTitleTag && (any (flip T.isInfixOf $ fromTagText $ sectionTitleTag) ["Notes", "NOTES"])
        isNoteSection [] = False

        splitLines :: [Tag T.Text] -> [[Tag T.Text]]
        splitLines = splitWhen (\tag -> isTagOpenName "br" tag || isTagOpenName "li" tag)

        convertLine :: [Tag T.Text] -> T.Text
        convertLine [] = ""
        convertLine (t:ts)
            | isTagOpenName "li" t = T.append "0." (innerText ts)
            | otherwise = innerText (t:ts)


parseRequirement :: [T.Text] -> [T.Text]
parseRequirement requirement = map parseSingleReq $ filter isReq requirement
    where
        isReq t = T.length t >= 7 &&
            not (any (flip T.isInfixOf $ t) ["First", "Second", "Third", "Higher"])

        parseSingleReq =
            T.pack . show .
            parseReqs .      -- Using parser for new Req type
            T.unpack .
            fromRight "" .
            P.parse getLineText "Reading a requirement line" .
            T.strip

        -- Strips the optional leading numbering (#.) from a line.
        getLineText :: Parser T.Text
        getLineText = do
            P.optional (P.digit >> P.char '.' >> P.space)
            parseUntil P.eof
