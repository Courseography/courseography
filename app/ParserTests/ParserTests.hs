{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module ParserTests.ParserTests
( reqTestSuite ) where

import Database.Requirement
import Test.HUnit (Test (..), assertEqual)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import WebParsing.ReqParser

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => Parser a -> String -> [(String, a)] -> Test
createTest parser label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ x ++ "),")
                                (Right y) (Parsec.parse parser "" x)) input

createReqParserTest :: String -> [(String, Req)] -> Test
createReqParserTest label input = TestLabel label $ TestList $ map (\(x, y) ->
                                  TestCase $ assertEqual ("for (" ++ x ++ "),") y (parseReqs x)) input

orInputs :: [(String, Req)]
orInputs = [
      ("CSC120H1/CSC148H1", OR [J "CSC120H1" "", J "CSC148H1" ""])
    , ("CSC108H1/CSC120H1/CSC148H1", OR [J "CSC108H1" "", J "CSC120H1" "", J "CSC148H1" ""])
    , ("FOR300H1/FOR301H1/FOR302H1", OR [J "FOR300H1" "", J "FOR301H1" "", J "FOR302H1" ""])
    , ("BIO130H1 or BIO206H5 or BIOB10H3", OR [J "BIO130H1" "", J "BIO206H5" "", J "BIOB10H3" ""])
    , ("CHM220H1 with a minimum grade of B, or CHM222H1", OR [GRADE "B" (J "CHM220H1" ""),J "CHM222H1" ""])
    ]

andInputs :: [(String, Req)]
andInputs = [
      ("CSC165H1, CSC236H1", AND [J "CSC165H1" "", J "CSC236H1" ""])
    , ("CSC120H1, CSC121H1, CSC148H1", AND [J "CSC120H1" "", J "CSC121H1" "", J "CSC148H1" ""])
    , ("CSC165H1 & CSC236H1", AND [J "CSC165H1" "", J "CSC236H1" ""])
    , ("BCH377H1; BCH378H1; and permission of Department", AND [J "BCH377H1" "", J "BCH378H1" "", RAW "permission of Department"])
    ]

andorInputs :: [(String, Req)]
andorInputs = [
      ("CSC148H1/CSC207H1, CSC165H1/CSC236H1", AND [OR [J "CSC148H1" "", J "CSC207H1" ""], OR [J "CSC165H1" "", J "CSC236H1" ""]])
    , ("COG250Y1 and one of: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", AND [J "COG250Y1" "", OR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 and one of either LIN232H1/LIN241H1 or JLP315H1/JLP374H1", AND [J "COG250Y1" "", OR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 + one of the following: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", AND [J "COG250Y1" "", OR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("CLA204H1 + 1 OF CLA160H1/CLA260H1", AND [J "CLA204H1" "", OR [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("BIO220H1 and at least one of EEB319H1/EEB321H1", AND [J "BIO220H1" "", OR [J "EEB319H1" "", J "EEB321H1" ""]])
    , ("CLA204H1 plus one of CLA160H1/CLA260H1", AND [J "CLA204H1" "", OR [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("ARH205H1/ ARH305H1, and one of ANT100Y1/ ANT200Y1/ ANT356H1", AND [OR [J "ARH205H1" "",J "ARH305H1" ""],OR [J "ANT100Y1" "",J "ANT200Y1" "",J "ANT356H1" ""]])
    ]

parInputs :: [(String, Req)]
parInputs = [
      ("(CSC148H1)", J "CSC148H1" "")
    , ("CSC108H1, (CSC165H1/CSC148H1)", AND [J "CSC108H1" "", OR [J "CSC165H1" "", J "CSC148H1" ""]])
    , ("(MAT135H1, MAT136H1)/ MAT137Y1", OR [AND [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" ""])
    , ("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", OR [J "CSC148H1" "", AND [OR [J "CSC108H1" "", J "CSC120H1" ""], OR [J "MAT137Y1" "", J "MAT157Y1" ""]]])
    , ("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", AND [OR [J "STA247H1" "", J "STA255H1" "", J "STA257H1" "", J "PSY201H1" "", J "ECO227Y1" ""], OR [AND [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" "", J "MAT157Y1" ""]])
    ]


fcesInputs :: [(String, Req)]
fcesInputs = [
      ("1.0 FCE from the following: (CSC148H1)", FCES "1.0" $ J "CSC148H1" "")
    , ("2.0 FCEs from CSC165H1/CSC148H1", FCES "2.0" $ OR [J "CSC165H1" "", J "CSC148H1" ""])
    , ("2 FCEs from: MAT135H1, MAT136H1/ MAT137Y1", FCES "2" $ AND [J "MAT135H1" "",OR [J "MAT136H1" "",J "MAT137Y1" ""]])
    , ("Completion of 4.0 FCEs", FCES "4.0" $ RAW "")
    , ("Completion of 4 FCE.", FCES "4" $ RAW "")
    , ("Completion of 9 FCEs", FCES "9" $ RAW "")
    , ("Completion of 9.0 credits or permission of the instructor", OR [FCES "9.0" (RAW ""), RAW "permission of the instructor"])
    , ("Completion of 9.0 credits. Permission of the instructor", AND [FCES "9.0" (RAW ""), RAW "Permission of the instructor"])
    , ("Completion of at least 9.0 FCE", FCES "9.0" $ RAW "")
    , ("Completion of a minimum of 4.0 FCEs", FCES "4.0" $ RAW "")
    , ("Completion of a minimum of 9 FCEs", FCES "9" $ RAW "")
    , ("Completion of 4.0 credits", FCES "4.0" $ RAW "")
    , ("at least 4.0 credits", FCES "4.0" $ RAW "")
    --, ("at least one additional 0.5 credit from a 300-/400-level course.", FCES "0.5" $ RAW "a 300-/400-level course.")
    --, ("at least 1.0 additional credit from 300-/400-level CSC/BCB courses", FCES "1.0" $ RAW "300-/400-level CSC/BCB courses")
    --, ("at least one 0.5 credit from a 400-level CSC/BCB course", FCES "0.5" $ RAW "a 400-level CSC/BCB course")
    , ("At least one 0.5 credit at the 400-level", FCES "0.5" $ RAW "the 400-level")
    --, ("At least one 0.5 credit at the 400-level or permission of the instructor", OR [FCES "0.5" (RAW "the 400-level"), RAW "permission of the instructor"])
    --, ("At least 1.0 credit must be at the 300-/400-level:", FCES "1.0" $ RAW "must be at the 300-/400-level:")
    , ("At least 1.5 credits at the 400-level", FCES "1.5" $ RAW "the 400-level")
    --, ("At least 1.5 credits from 400-level CSC or BCB courses.", FCES "1.5" $ RAW "400-level CSC or BCB courses.")
    --, ("1.0 credits at the 300-level or higher", FCES "1.0" (RAW "the 300-level or higher"))
    --, ("1.0 credit in HPS or HIS", FCES "1.0" (RAW "HPS or HIS"))
    , ("1.0 credits or 1.0 credit in Canadian Studies", OR [FCES "1.0" (RAW ""),FCES "1.0" (RAW "Canadian Studies")])
    --, ("1.0 credits, CSE240H1/ NEW240Y1, CSE342H1, an additional 0.5 credit at the 300+ level from the Critical Studies", AND [FCES "1.0" (RAW ""),OR [J "CSE240H1" "",J "NEW240Y1" ""],J "CSE342H1" "",FCES "0.5" (RAW "the 300+ level from the Critical Studies")])
    ]

gradeBefInputs :: [(String, Req)]
gradeBefInputs = [
      ("minimum mark of A- in CSC236H1", GRADE "A-" $ J "CSC236H1" "")
    , ("minimum grade of 75% CSC236H1", GRADE "75" $ J "CSC236H1" "")
    , ("minimum of 75% CSC236H1", GRADE "75" $ J "CSC236H1" "")
    , ("minimum (75%) CSC236H1", GRADE "75" $ J "CSC236H1" "")
    , ("A grade of 75% in CSC236H1", GRADE "75" $ J "CSC236H1" "")
    , ("At least C+ in CSC236H1", GRADE "C+" $ J "CSC236H1" "")
    , ("A C+ in CSC236H1", GRADE "C+" $ J "CSC236H1" "")
    , ("Minimum of 75% CSC236H1", GRADE "75" $ J "CSC236H1" "")
    , ("Grade of C+ in CSC236H1", GRADE "C+" $ J "CSC236H1" "")
    , ("A final grade of C+ in CSC236H1", GRADE "C+" $ J "CSC236H1" "")
    , ("75% in CSC236H1", GRADE "75" $ J "CSC236H1" "")
    ]

gradeAftInputs :: [(String, Req)]
gradeAftInputs = [
      ("CSC236H1 75%", GRADE "75" $ J "CSC236H1" "")
    , ("CSC236H1 (75%)", GRADE "75" $ J "CSC236H1" "")
    , ("CSC236H1(75%)", GRADE "75" $ J "CSC236H1" "")
    , ("CSC263H1 (C+)", GRADE "C+" $ J "CSC263H1" "")
    , ("CSC263H1 B-", GRADE "B-" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum grade of 60%", GRADE "60" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum mark of B-", GRADE "B-" $ J "CSC263H1" "")
    , ("CSC236H1 (at least 75% or more)", GRADE "75" $ J "CSC236H1" "")
    , ("CSC236H1 ( 75% or higher )", GRADE "75" $ J "CSC236H1" "")
    , ("CSC263H1 with a minimum grade of 60% or more", GRADE "60" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum grade of 60% or higher / CSC236H1 (75%)", OR [GRADE "60" $ J "CSC263H1" "", GRADE "75" $ J "CSC236H1" ""])
    , ("A in CSC236H1", GRADE "A" $ J "CSC236H1" "")
    ]

artSciInputs :: [(String, Req)]
artSciInputs = [
      ("BIO220H1 (ecology and evolutionary biology)", J "BIO220H1" "ecology and evolutionary biology")
    , ("EEB223H1/ STA220H1 (recommended)/ STA257H1 (recommended)", OR [J "EEB223H1" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    , ("EEB223H1 (ecology and evo), STA220H1 (recommended)/ STA257H1 (recommended)", AND [J "EEB223H1" "ecology and evo",OR [J "STA220H1" "recommended",J "STA257H1" "recommended"]])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (recommended)/ STA257H1", OR [J "EEB223H1" "ecology and evo",J "STA220H1" "recommended",J "STA257H1" ""])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (B-)/ STA257H1", OR [J "EEB223H1" "ecology and evo", GRADE "B-" $ J "STA220H1" "", J "STA257H1" ""])
    , ("0.5 FCE from: EEB225H1 (recommended)/ STA220H1 (B-)/ STA257H1/  STA288H1/ GGR270H1/ PSY201H1", FCES "0.5" $ OR [J "EEB225H1" "recommended", GRADE "B-" $ J "STA220H1" "", J "STA257H1" "", J "STA288H1" "", J "GGR270H1" "", J "PSY201H1" ""])
    , ("MATB23H3/STA220H1 (recommended)/STA257H1 (recommended)", OR [J "MATB23H3" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    ]

programOrInputs :: [(String, Req)]
programOrInputs = [
      ("Admission to Vic One", PROGRAM "Vic One")
    , ("Enrolment in the International Relations program or in a History major or specialist program, or permission of instructor", OR [PROGRAM "International Relations",PROGRAM "History major",PROGRAM "History specialist", RAW "permission of instructor"])
    , ("Enrolment in the International Relations program or in a History or Political Science major or specialist program", OR [PROGRAM "International Relations",PROGRAM "History major",PROGRAM "History specialist",PROGRAM "Political Science major",PROGRAM "Political Science specialist"])
    --, ("Enrolment in ASMAJ1618. A student must be in third or fourth year.", AND [PROGRAM "ASMAJ1618",RAW "A student must be in third or fourth year."])
    , ("Enrolment in the PSY Research Specialist program, and PSY309H1, and one of PSY319H1/ PSY329H1/ PSY339H1", AND [PROGRAM "PSY Research Specialist",J "PSY309H1" "",OR [J "PSY319H1" "",J "PSY329H1" "",J "PSY339H1" ""]])
    , ("70% in SOC212H1 and enrolment in Sociology program", AND [GRADE "70" (J "SOC212H1" ""),PROGRAM "Sociology"])
    , ("(70% in SOC212H1 and enrolment in Sociology program)", AND [GRADE "70" (J "SOC212H1" ""),PROGRAM "Sociology"])
    , ("Admission to International Relations Major or Specialist program", OR [PROGRAM "International Relations Major",PROGRAM "International Relations Specialist"])
    , ("Instructor’s permission required for admission to course", RAW "Instructor\8217s permission required for admission to course")
    , ("MGT100H1, or enrolment in the Actuarial Science Specialist or Major", OR [J "MGT100H1" "",PROGRAM "Actuarial Science Specialist",PROGRAM "Actuarial Science Major"])
    , ("Enrolment in Psychology Minor", PROGRAM "Psychology Minor")
    , ("Enrolment in History major or permission of instructor", OR [PROGRAM "History major",RAW "permission of instructor"])
    , ("enrolment in a science, mathematics, or engineering program", OR [PROGRAM "science",PROGRAM "mathematics",PROGRAM "or engineering"])
    , ("enrolment in a science, mathematics, or engineering program, or permission from instructor", OR [PROGRAM "science",PROGRAM "mathematics",PROGRAM "or engineering", RAW "permission from instructor"])
    ]

noPrereqInputs :: [(String, Req)]
noPrereqInputs = [
      ("", NONE)
    , ("None", NONE)
    , ("none", NONE)
    , ("No", NONE)
    , ("no", NONE)
    ]

orTests :: Test
orTests = createTest categoryParser "Basic or Requirement" orInputs

andTests :: Test
andTests = createTest categoryParser "Basic and Requirement" andInputs

andorTests :: Test
andorTests = createTest categoryParser "Basic and-or-mixed Requirement" andorInputs

parTests :: Test
parTests = createTest categoryParser "Basic and-or-parenthesized Requirement" parInputs

fcesTests:: Test
fcesTests = createTest categoryParser "Basic fces Requirement" fcesInputs

-- Outdated
-- fromParTests :: Test
-- fromParTests = createTest categoryParser "Paranthesized From Requirements with integer or float fces" fromParInputs

gradeBefTests :: Test
gradeBefTests = createTest categoryParser "Basic grade requirements which come before." gradeBefInputs

gradeAftTests :: Test
gradeAftTests = createTest categoryParser "Basic grade requirements, where grades come after." gradeAftInputs

artSciTests :: Test
artSciTests = createTest categoryParser "Arts and Science requirements from Christine's output" artSciInputs

programOrTests :: Test
programOrTests = createTest categoryParser "program requirements" programOrInputs

noPrereqTests :: Test
noPrereqTests = createReqParserTest "No prerequisites required" noPrereqInputs

-- functions for running tests in REPL
reqTestSuite :: Test
reqTestSuite = TestLabel "ReqParser tests" $ TestList [orTests, andTests, andorTests, parTests, fcesTests, gradeBefTests, gradeAftTests, artSciTests, programOrTests, noPrereqTests]
