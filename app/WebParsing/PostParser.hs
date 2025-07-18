module WebParsing.PostParser
    ( addPostToDatabase
    , postInfoParser
    , pruneHtml
    , getPostType
    ) where

import Control.Monad.Trans (liftIO)
import Data.Either (fromRight)
import Data.Functor (void)
import Data.List (find)
import Data.List.Split (keepDelimsL, split, splitWhen, whenElt)
import Data.Text (strip)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.DataType (PostType (..))
import Database.Persist (insertUnique)
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import WebParsing.ParsecCombinators (parseUntil)
import WebParsing.ReqParser (parseReqs)


addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    let fullPostName = maybe "" (strip . fromTagText) $ find isTagText programElements
        postDescHtml = partitions isDescriptionSection programElements
        descriptionText = case postDescHtml of
            [] -> T.empty
            (x:_) -> renderTags x
        postReqHtml = sections isRequirementSection programElements
        requirementLines = if null postReqHtml then [] else pruneHtml $ last postReqHtml
        requirements = concatMap parseRequirement $ reqHtmlToLines requirementLines
    liftIO $ print fullPostName

    case P.parse postInfoParser "POSt information" fullPostName of
        Left _ -> return ()
        Right (department, code) -> do
            currTime <- liftIO getCurrentTime
            postExists <- insertUnique Post {
                postName = getPostType code department,
                postDepartment = department,
                postCode = code,
                postDescription = descriptionText,
                postRequirements = renderTags requirementLines,
                postCreated = currTime,
                postModified = currTime
                }
            case postExists of
                Just key ->
                    mapM_ (insert_ . PostCategory key) requirements
                Nothing -> return ()
    where
        isDescriptionSection tag = tagOpenAttrNameLit "div" "class" (T.isInfixOf "views-field-body") tag || isRequirementSection tag
        isRequirementSection tag = tagOpenAttrNameLit "div" "class" (T.isInfixOf "views-field-field-enrolment-requirements") tag || tagOpenAttrNameLit "div" "class" (T.isInfixOf "views-field-field-completion-requirements") tag


-- | Parse a Post value from its title.
-- Titles are usually of the form "Actuarial Science Major (Science Program)".
postInfoParser :: Parser (T.Text, T.Text)
postInfoParser = do
    deptName <- P.manyTill P.anyChar $ P.choice $ map (P.try . P.lookAhead) [
        void postCodeParser,
        P.eof
        ]
    code <- postCodeParser P.<|> return T.empty

    return (T.pack deptName, code)

-- | Extracts the post type (eg. major) from a post code if it is non-empty,
-- | or from a dept name otherwise
getPostType :: T.Text -> T.Text -> PostType
getPostType "" deptName = getPostTypeFromName deptName
getPostType code _ = getPostTypeFromCode code

-- | Extracts the post type (eg. major) from a post name (eg. "Biology Specialist")
getPostTypeFromName :: T.Text -> PostType
getPostTypeFromName deptName
    | T.isInfixOf "Specialist" deptName = Specialist
    | T.isInfixOf "Major" deptName = Major
    | T.isInfixOf "Minor" deptName = Minor
    | T.isInfixOf "Focus" deptName = Focus
    | T.isInfixOf "Certificate" deptName = Certificate
    | otherwise = Other

-- | Extracts the post type (eg. major) from a post code (eg. ASMAJ1689)
getPostTypeFromCode :: T.Text -> PostType
getPostTypeFromCode = abbrevToPost . T.take 3 . T.drop 2

-- | Maps the post type abbreviations to their corresponding PostType
abbrevToPost :: T.Text -> PostType
abbrevToPost "SPE" = Specialist
abbrevToPost "MAJ" = Major
abbrevToPost "MIN" = Minor
abbrevToPost "FOC" = Focus
abbrevToPost "CER" = Certificate
abbrevToPost _ = Other

-- | Parser for a post code (eg. ASFOC1689A)
postCodeParser :: Parser T.Text
postCodeParser = do
    _ <- P.many1 P.space >> P.char '-' >> P.many1 P.space
    code <- P.count 5 P.letter
    num <- P.count 4 P.digit
    variant <- P.many P.letter
    return $ T.pack $ code ++ num ++ variant

-- | Prunes all the attributes (eg. class, href) in the html except for the style.
-- | Removes all <a></a> tags
pruneHtml :: [Tag T.Text] -> [Tag T.Text]
pruneHtml [] = []
pruneHtml ((TagOpen "a" _):xs) = pruneHtml xs
pruneHtml ((TagClose "a"):xs) = pruneHtml xs
pruneHtml ((TagOpen tag attrs):xs) = (TagOpen tag [style | style@("style", _) <- attrs]) : pruneHtml xs
pruneHtml (x:xs) = x : pruneHtml xs

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
            any (flip T.isInfixOf $ fromTagText tag)
                ["First", "Second", "Third", "Higher", "Recommended Courses:", "Notes", "NOTES"]

        isNoteSection :: [Tag T.Text] -> Bool
        isNoteSection (sectionTitleTag:_) =
            isTagText sectionTitleTag && any (flip T.isInfixOf $ fromTagText sectionTitleTag) ["Notes", "NOTES"]
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
            not (any (`T.isInfixOf` t) ["First", "Second", "Third", "Higher"])

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
            P.optional $ P.try (P.digit >> P.char '.' >> P.space)
            parseUntil P.eof
