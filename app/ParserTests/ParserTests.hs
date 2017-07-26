{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module ParserTests.ParserTests
( reqTestSuite ) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Database.Requirement
import WebParsing.ReqParser
import Test.HUnit ( assertEqual, Test(..) )

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => Parser a -> String -> [(String, a)] -> Test
createTest parser label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ x ++ "),")
                                (Right y) (Parsec.parse parser "" x)) input

-- categoryParser Tests
orInputs :: [(String, Req)]
orInputs = [
      ("CSC120H1/CSC148H1", OR [J "CSC120H1", J "CSC148H1"])
    , ("CSC108H1/CSC120H1/CSC148H1", OR [J "CSC108H1", J "CSC120H1", J "CSC148H1"])
    ]

andInputs :: [(String, Req)]
andInputs = [
      ("CSC165H1, CSC236H1", AND [J "CSC165H1", J "CSC236H1"])
    , ("CSC120H1, CSC121H1, CSC148H1", AND [J "CSC120H1", J "CSC121H1", J "CSC148H1"])
    ]

andorInputs :: [(String, Req)]
andorInputs = [
      ("CSC148H1/CSC207H1, CSC165H1/CSC236H1", AND [OR [J "CSC148H1", J "CSC207H1"], OR [J "CSC165H1", J "CSC236H1"]])
    ]

parInputs :: [(String, Req)]
parInputs = [
      ("(CSC148H1)", J "CSC148H1")
    , ("CSC108H1, (CSC165H1/CSC148H1)", AND [J "CSC108H1", OR [J "CSC165H1", J "CSC148H1"]])
    , ("(MAT135H1, MAT136H1)/ MAT137Y1", OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1"])
    , ("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", OR [J "CSC148H1", AND [OR [J "CSC108H1", J "CSC120H1"], OR [J "MAT137Y1", J "MAT157Y1"]]])
    , ("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", AND [OR [J "STA247H1", J "STA255H1", J "STA257H1", J "PSY201H1", J "ECO227Y1"], OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1", J "MAT157Y1"]])
    ]

fromParInputs :: [(String, Req)]
fromParInputs = [
      ("1.0 FCE from the following: (CSC148H1)", FCES "1.0" $ J "CSC148H1")
    , ("2.0 FCEs from CSC165H1/CSC148H1", FCES "2.0" $ OR [J "CSC165H1", J "CSC148H1"])
    , ("2 FCEs from: MAT135H1, MAT136H1/ MAT137Y1", FCES "2" $ AND [J "MAT135H1",OR [J "MAT136H1",J "MAT137Y1"]])
    ]


-- TODO: No more "before" reqs in new artsci calendar except first test case.
gradeBefInputs :: [(String, Req)]
gradeBefInputs = [
      ("minimum mark of A- in CSC236H1", GRADE "A-" $ J "CSC236H1") 
    , ("minimum grade of 75% CSC236H1", GRADE "75" $ J "CSC236H1")
    , ("minimum of 75% CSC236H1", GRADE "75" $ J "CSC236H1")
    , ("minimum (75%) CSC236H1", GRADE "75" $ J "CSC236H1")
    ]

gradeAftInputs :: [(String, Req)]
gradeAftInputs = [
      ("CSC236H1 75%", GRADE "75" $ J "CSC236H1")
    , ("CSC236H1 (75%)", GRADE "75" $ J "CSC236H1")
    , ("CSC236H1(75%)", GRADE "75" $ J "CSC236H1")
    , ("CSC263H1 (C+)", GRADE "C+" $ J "CSC263H1")
    , ("CSC263H1 B-", GRADE "B-" $ J "CSC263H1") 
    , ("CSC263H1 with a minimum grade of 60%", GRADE "60" $ J "CSC263H1") 
    , ("CSC263H1 with a minimum mark of B-", GRADE "B-" $ J "CSC263H1")
    ]

artSciInputs :: [(String, Req)]
artSciInputs = [
      ("1.0 full course or its equivalent from HIS230H1/ HIS231H1/ NEW220H1/ NEW221H1/ NEW225H1/ NEW226H1", FCES "1.0" (OR [J "HIS230H1",J "HIS231H1",J "NEW220H1",J "NEW221H1",J "NEW225H1",J "NEW226H1"]))
    , ("5 FCEs of any of the following: LIN232H1; LIN241H1; JLP315H1; LIN331H1; LIN341H1; JLP374H1", J "")
    , ("(ECO101H1, ECO102H1)/ECO100Y1, RSM100H1/ MGT100H1/ RSM100Y1", AND [OR [AND [J "ECO101H1",J "ECO102H1"],J "ECO100Y1"],OR [J "RSM100H1",J "MGT100H1",J "RSM100Y1"]])
    , ("CAS310H1 and CAS320H1", AND [J "CAS310H1",J "CAS320H1"])
    ]

{-|
"FALSE POSITIVE?"

Right (AND [FCES "5" (GRADE "1" (J "LIN232H1")),GRADE "1" (J "JLP315H1"),GRADE "1" (J "LIN341H1")]
    vs.
Right (FCES "5" [J "LIN232H1", J "LIN241H1", J "JLP315H1", J "LIN331H1", J "LIN341H1", J "JLP374H1"])

How do we want to handle semicolons?



"0.5 FCE in statistics from: EEB225H1 (recommended)/ STA220H1/ STA257H1/  STA288H1/ GGR270H1/ PSY201H1","0.5 FCE in core evolution from: EEB318H1, EEB323H1, EEB362H1"

_ FCE [in ?] from Req
        ? is a program/group.. should this be dealt with in my level..
        can do a try, with highest precedence trying to parse whatever is between FCE and from; else use the hard-coded methods?
            - Can store whatever is parsed in between as the Program/Group?

-}

{-
2.0 FCEs: BIO220H1 (ecology and evolutionary biology); ENV234H1 (cannot be substituted with EEB375H1 for this requirement), ENV334H1 (environmental biology)

finding new pattern:

Req (?)
?: raw information to be parsed using raw parser?.. should we add attribute to all reqs that holds such info and use rawParser to get whatever is within
    the paranthesese instead of having the rawParser be the lowest level parser?

-}




{-
"Third Year: 1.0 FCE from FOR300H1, FOR301H1, FOR302H1, FOR303H1, FOR305H1, FOR306H1, FOR307H1, FOR310H1","Fourth Year: FOR400Y1"

FROM parser should parse for single courses? How do we want to parse/represent this req

GROUP "Third Year" $ FCES "1.0" $ FROM [J "FOR300H1", J "FOR301H1", J "FOR302H1", J "FOR303H1", J "FOR305H1", J "FOR306H1", J "FOR307H1", J "FOR310H1"]

THIS SEEMS COMPLETELY MISLEADING, THE CONNECTIONS ARE VERY IMPORTANT BC ANDS AND ORS ARE NATURAL LANGUAGE
"1.0 FCES FROM THIS, AND THAT, AND THAT, AND THAT OR THAT, AND THAT." VS "1.0 FCES FROM THIS, THIS, THIS, THIS, THIS.."

GROUP "Third Year" $ FCES "1.0" $ FROM [AND [J "FOR300H1", J "FOR301H1", J "FOR302H1", J "FOR303H1", J "FOR305H1", J "FOR306H1", J "FOR307H1", J "FOR310H1"]]
-}



orTests :: Test
orTests = createTest categoryParser "Basic or Requirement" orInputs

andTests :: Test
andTests = createTest categoryParser "Basic and Requirement" andInputs

andorTests :: Test
andorTests = createTest categoryParser "Basic and-or-mixed Requirement" andorInputs

parTests :: Test
parTests = createTest categoryParser "Basic and-or-parenthesized Requirement" parInputs

fromParTests :: Test
fromParTests = createTest categoryParser "Paranthesized From Requirements with integer or float fces" fromParInputs

gradeBefTests :: Test
gradeBefTests = createTest categoryParser "Basic grade requirements which come before." gradeBefInputs

gradeAftTests :: Test
gradeAftTests = createTest categoryParser "Basic grade requirements, where grades come after." gradeAftInputs

artSciTests :: Test
artSciTests = createTest categoryParser "Arts and Science requirements from Christine's output" artSciInputs

-- functions for running tests in REPL
reqTestSuite :: Test
reqTestSuite = TestLabel "ReqParser tests" $ TestList [orTests, andTests, andorTests, parTests, fromParTests, gradeBefTests, gradeAftTests, artSciTests]
