{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParsecCombinators
    (getCourseFromTag,
     findCourseFromTag,
     getPostType,
     extractPostType,
     findPostType,
     getDepartmentName,
     isDepartmentName,
     parsingAlgoOne) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.Text as T
import Data.Functor.Identity

getCourseFromTag courseTag = do
    let course = P.parse findCourseFromTag "(source)" courseTag
    case course of
        Right name -> name
        Left _ -> ""

findCourseFromTag :: P.Parsec String () String
findCourseFromTag = do
    P.manyTill P.anyChar (P.char '#') 
    P.many1 P.anyChar

-- Post Parsing

getPostType :: T.Text -> String
getPostType postCode = 
    let codeSection = extractPostType (T.unpack postCode)
    in
        case codeSection of
            "SPE" -> "Specialist"
            "MAJ" -> "Major"
            "MIN" ->  "Minor"

extractPostType :: [Char] -> [Char]
extractPostType postCode = do
    let parsed = P.parse findPostType "(source)" postCode
    case parsed of 
        Right name -> name
        Left _ -> ""

findPostType :: P.Parsec String () String
findPostType = do
   P.string "AS"
   P.many1 P.letter

getDepartmentName :: [Char] -> T.Text -> [Char]
getDepartmentName fullPostName postType = do
    let parsed = P.parse (isDepartmentName (T.unpack postType)) "(source)" fullPostName
    case parsed of 
        Right name -> name
        Left _ -> ""

isDepartmentName ::  [Char] -> P.Parsec String () String
isDepartmentName postType = P.manyTill P.anyChar (P.try (P.string postType))

-- Post Category Parsing

parsingAlgoOne :: P.Parsec String () [String]
parsingAlgoOne = do
    getRequirements 
    splitPrereqText

getRequirements ::  P.Parsec String () String
getRequirements =  do
    P.manyTill P.anyChar (P.try (P.string "Program Course Requirements:"))

splitPrereqText :: P.Parsec String () [String]
splitPrereqText = do
    P.manyTill P.anyChar (P.try (P.string "First Year"))
    P.manyTill getCategory ((P.try (P.string "Notes")) <|> (P.try (P.string "NOTES")))

getCategory :: P.Parsec String () String
getCategory = do
    P.manyTill P.anyChar (P.try categorySeperator)

categorySeperator = do
    P.oneOf ";\r\n\160"

