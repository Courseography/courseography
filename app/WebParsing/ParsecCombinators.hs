module WebParsing.ParsecCombinators
    (getCourseFromTag,
     findCourseFromTag,
     text,
     parseUntil) where

import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)


getCourseFromTag :: T.Text -> T.Text
getCourseFromTag courseTag =
    let course = P.parse findCourseFromTag "(source)" courseTag
    in
        case course of
            Right courseName -> courseName
            Left _ -> T.pack ""

findCourseFromTag :: Parser T.Text
findCourseFromTag = do
    _ <- P.string "/course/"
    parsed <- P.many1 P.anyChar
    return $ T.pack parsed

parseUntil :: Parser a -> Parser T.Text
parseUntil parser = do
    parsed <- P.manyTill P.anyChar (P.try parser)
    return $ T.pack parsed

text :: T.Text -> Parser T.Text
text someText = do
    parsed <- mapM P.char (T.unpack someText)
    return $ T.pack parsed
