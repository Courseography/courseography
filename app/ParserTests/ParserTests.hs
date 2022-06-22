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
      ("CSC120H1/CSC148H1", REQOR [J "CSC120H1" "", J "CSC148H1" ""])
    , ("CSC108H1/CSC120H1/CSC148H1", REQOR [J "CSC108H1" "", J "CSC120H1" "", J "CSC148H1" ""])
    , ("FOR300H1/FOR301H1/FOR302H1", REQOR [J "FOR300H1" "", J "FOR301H1" "", J "FOR302H1" ""])
    , ("BIO130H1 or BIO206H5 or BIOB10H3", REQOR [J "BIO130H1" "", J "BIO206H5" "", J "BIOB10H3" ""])
    , ("CHM220H1 with a minimum grade of B, or CHM222H1", REQOR [GRADE "B" (J "CHM220H1" ""),J "CHM222H1" ""])
    ]

andInputs :: [(String, Req)]
andInputs = [
      ("CSC165H1, CSC236H1", REQAND [J "CSC165H1" "", J "CSC236H1" ""])
    , ("CSC120H1, CSC121H1, CSC148H1", REQAND [J "CSC120H1" "", J "CSC121H1" "", J "CSC148H1" ""])
    , ("CSC165H1 & CSC236H1", REQAND [J "CSC165H1" "", J "CSC236H1" ""])
    , ("BCH377H1; BCH378H1; and permission of Department", REQAND [J "BCH377H1" "", J "BCH378H1" "", RAW "permission of Department"])
    , ("CSC111H1. Permission of department", REQAND [J "CSC111H1" "", RAW "Permission of department"])
    , ("CSC110H1, an additional permission of department", REQAND [J "CSC110H1" "", RAW "permission of department"])
    , ("CSC110H1, additional 2.0 credits from CSC", REQAND [J "CSC110H1" "", FCES 2.0 (DEPARTMENT "CSC")])
    ]

andorInputs :: [(String, Req)]
andorInputs = [
      ("CSC148H1/CSC207H1, CSC165H1/CSC236H1", REQAND [REQOR [J "CSC148H1" "", J "CSC207H1" ""], REQOR [J "CSC165H1" "", J "CSC236H1" ""]])
    , ("COG250Y1 and one of: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", REQAND [J "COG250Y1" "", REQOR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 and one of either LIN232H1/LIN241H1 or JLP315H1/JLP374H1", REQAND [J "COG250Y1" "", REQOR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 + one of the following: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", REQAND [J "COG250Y1" "", REQOR [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("CLA204H1 + 1 OF CLA160H1/CLA260H1", REQAND [J "CLA204H1" "", REQOR [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("BIO220H1 and at least one of EEB319H1/EEB321H1", REQAND [J "BIO220H1" "", REQOR [J "EEB319H1" "", J "EEB321H1" ""]])
    , ("CLA204H1 plus one of CLA160H1/CLA260H1", REQAND [J "CLA204H1" "", REQOR [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("ARH205H1/ ARH305H1, and one of ANT100Y1/ ANT200Y1/ ANT356H1", REQAND [REQOR [J "ARH205H1" "",J "ARH305H1" ""],REQOR [J "ANT100Y1" "",J "ANT200Y1" "",J "ANT356H1" ""]])
    ]

parInputs :: [(String, Req)]
parInputs = [
      ("(CSC148H1)", J "CSC148H1" "")
    , ("CSC108H1, (CSC165H1/CSC148H1)", REQAND [J "CSC108H1" "", REQOR [J "CSC165H1" "", J "CSC148H1" ""]])
    , ("(MAT135H1, MAT136H1)/ MAT137Y1", REQOR [REQAND [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" ""])
    , ("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", REQOR [J "CSC148H1" "", REQAND [REQOR [J "CSC108H1" "", J "CSC120H1" ""], REQOR [J "MAT137Y1" "", J "MAT157Y1" ""]]])
    , ("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", REQAND [REQOR [J "STA247H1" "", J "STA255H1" "", J "STA257H1" "", J "PSY201H1" "", J "ECO227Y1" ""], REQOR [REQAND [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" "", J "MAT157Y1" ""]])
    ]


fcesInputs :: [(String, Req)]
fcesInputs = [
      ("1.0 FCE from the following: (CSC148H1)", FCES 1.0 $ REQUIREMENT $ J "CSC148H1" "")
    , ("2.0 FCEs from CSC165H1/CSC148H1", FCES 2.0 $ REQUIREMENT $ REQOR [J "CSC165H1" "", J "CSC148H1" ""])
    , ("2.0 FCEs in CSC165H1/CSC148H1", FCES 2.0 $ REQUIREMENT $ REQOR [J "CSC165H1" "", J "CSC148H1" ""])
    , ("2 FCEs from: MAT135H1, MAT136H1/ MAT137Y1", FCES 2.0 $ REQUIREMENT $ REQAND [J "MAT135H1" "",REQOR [J "MAT136H1" "",J "MAT137Y1" ""]])
    , ("Completion of 4.0 FCEs", FCES 4.0 $ REQUIREMENT $ RAW "")
    , ("Completion of 4 FCE.", FCES 4.0 $ REQUIREMENT $ RAW "")
    , ("Completion of 9 FCEs", FCES 9.0 $ REQUIREMENT $ RAW "")
    , ("Completion of 9.0 credits or permission of the instructor", REQOR [FCES 9.0 (REQUIREMENT $ RAW ""), RAW "permission of the instructor"])
    , ("Completion of 9.0 credits. Permission of the instructor", REQAND [FCES 9.0 (REQUIREMENT $ RAW ""), RAW "Permission of the instructor"])
    , ("Completion of at least 9.0 FCE", FCES 9.0 $ REQUIREMENT $ RAW "")
    , ("Completion of a minimum of 4.0 FCEs", FCES 4.0 $ REQUIREMENT $ RAW "")
    , ("Completion of a minimum of 9 FCEs", FCES 9.0 $ REQUIREMENT $ RAW "")
    , ("Completion of 4.0 credits", FCES 4.0 $ REQUIREMENT $ RAW "")
    , ("at least 4.0 credits", FCES 4.0 $ REQUIREMENT $ RAW "")
    , ("At least one 0.5 credit at the 400-level", FCES 0.5 (LEVEL "400"))
    , ("At least 1.5 credits at the 400-level", FCES 1.5 (LEVEL "400"))
    , ("2.0 credits from 300 level CSC courses", FCES 2.0 (MODAND [LEVEL "300", DEPARTMENT "CSC"]))
    , ("1.0 credits at the 300-level or higher", FCES 1.0 (LEVEL "300+"))
    , ("1.0 credits at the 300+ level", FCES 1.0 (LEVEL "300+"))
    , ("1.0 credits of 300+ level CSC courses", FCES 1.0 (MODAND [LEVEL "300+", DEPARTMENT "CSC"]))
    , ("1.0 credits at the 300-level or higher from CSC courses", FCES 1.0 (MODAND [LEVEL "300+", DEPARTMENT "CSC"]))
    , ("1.0 credit at the 300-level from Group B: Evolutionary", FCES 1.0 (MODAND [LEVEL "300", DEPARTMENT "Group B: Evolutionary"]))
    , ("ANT203Y1 and a 0.5 credit 300+ level course from Group B: Evolutionary", REQAND [J "ANT203Y1" "", FCES 0.5 (MODAND [LEVEL "300+", DEPARTMENT "Group B: Evolutionary"])])
    , ("ANT253H1 and 1.0 credit at the 300-level", REQAND [J "ANT253H1" "", FCES 1.0 (LEVEL "300")])
    , ("1.0 credit in FRE at the 200-level", FCES 1.0 (MODAND [DEPARTMENT "FRE", LEVEL "200"]))
    , ("1.0 CLA credit at the 300-level", FCES 1.0 (MODAND [LEVEL "300", DEPARTMENT "CLA"]))
    , ("14.0 credits, 3.0 credits in Anthropology", REQAND [FCES 14.0 (REQUIREMENT $ RAW ""), FCES 3.0 (DEPARTMENT "Anthropology")])
    , ("Completion of 14.0 credits including PSL300H1, PSL301H1, and 0.5 HMB credit at the 300-level", REQAND [FCES 14.0 (REQUIREMENT $ RAW ""), J "PSL300H1" "", J "PSL301H1" "", FCES 0.5 $ MODAND [LEVEL "300", DEPARTMENT "HMB"]])
    , ("1.0 credits, CSE240H1/ NEW240Y1, CSE342H1, an additional 0.5 credit at the 300+ level from the Critical Studies", REQAND [FCES 1.0 (REQUIREMENT $ RAW ""),REQOR [J "CSE240H1" "",J "NEW240Y1" ""],J "CSE342H1" "", FCES 0.5 (MODAND [LEVEL "300+", DEPARTMENT "Critical Studies"])])
    , ("1.0 credits or 1.0 credit in Canadian Studies", REQOR [FCES 1.0 (REQUIREMENT $ RAW ""),FCES 1.0 (DEPARTMENT "Canadian Studies")])
    , ("NEW240Y1, an additional 0.5 credits at the 300 level from the Critical Studies", REQAND [J "NEW240Y1" "", FCES 0.5 (MODAND [LEVEL "300", DEPARTMENT "Critical Studies"])])
    , ("At least one 0.5 credit at the 400-level or permission of the instructor", REQOR [FCES 0.5 (LEVEL "400"), RAW "permission of the instructor"])
    , ("0.5 credit in HPS", FCES 0.5 (DEPARTMENT "HPS"))
    , ("1.0 credit in MST courses and 0.5 credit in HIS", REQAND [FCES 1.0 (DEPARTMENT "MST"), FCES 0.5 (DEPARTMENT "HIS")])
    , ("2.0 ENG credits", FCES 2.0 (DEPARTMENT "ENG"))
    , ("Any 9.0 credits", FCES 9.0 (REQUIREMENT $ RAW ""))
    , ("2.0 ENG credits and any 4.0 credits", REQAND [FCES 2.0 (DEPARTMENT "ENG"), FCES 4.0 (REQUIREMENT $ RAW "")])
    , ("9.0 credits in any field", FCES 9.0 (REQUIREMENT $ RAW ""))
    , ("9.0 credits in any subject", FCES 9.0 (REQUIREMENT $ RAW ""))
    , ("1.0 credits of CSC courses", FCES 1.0 (DEPARTMENT "CSC"))
    , ("1.0 credits from the CSC courses", FCES 1.0 (DEPARTMENT "CSC"))
    , ("1.0 credits at the 400-level", FCES 1.0 (LEVEL "400"))
    , ("a 0.5 credit 300 level course", FCES 0.5 (LEVEL "300"))
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
    , ("CSC263H1 with a minimum grade of 60% or higher / CSC236H1 (75%)", REQOR [GRADE "60" $ J "CSC263H1" "", GRADE "75" $ J "CSC236H1" ""])
    , ("A in CSC236H1", GRADE "A" $ J "CSC236H1" "")
    ]

artSciInputs :: [(String, Req)]
artSciInputs = [
      ("BIO220H1 (ecology and evolutionary biology)", J "BIO220H1" "ecology and evolutionary biology")
    , ("EEB223H1/ STA220H1 (recommended)/ STA257H1 (recommended)", REQOR [J "EEB223H1" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    , ("EEB223H1 (ecology and evo), STA220H1 (recommended)/ STA257H1 (recommended)", REQAND [J "EEB223H1" "ecology and evo",REQOR [J "STA220H1" "recommended",J "STA257H1" "recommended"]])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (recommended)/ STA257H1", REQOR [J "EEB223H1" "ecology and evo",J "STA220H1" "recommended",J "STA257H1" ""])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (B-)/ STA257H1", REQOR [J "EEB223H1" "ecology and evo", GRADE "B-" $ J "STA220H1" "", J "STA257H1" ""])
    , ("0.5 FCE from: EEB225H1 (recommended)/ STA220H1 (B-)/ STA257H1/  STA288H1/ GGR270H1/ PSY201H1", FCES 0.5 $ REQUIREMENT $ REQOR [J "EEB225H1" "recommended", GRADE "B-" $ J "STA220H1" "", J "STA257H1" "", J "STA288H1" "", J "GGR270H1" "", J "PSY201H1" ""])
    , ("MATB23H3/STA220H1 (recommended)/STA257H1 (recommended)", REQOR [J "MATB23H3" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    ]

programOrInputs :: [(String, Req)]
programOrInputs = [
      ("Admission to Vic One", PROGRAM "Vic One")
    , ("Enrolment in the International Relations program or in a History major or specialist program, or permission of instructor", REQOR [PROGRAM "International Relations",PROGRAM "History major",PROGRAM "History specialist", RAW "permission of instructor"])
    , ("Enrolment in the International Relations program or in a History or Political Science major or specialist program", REQOR [PROGRAM "International Relations",PROGRAM "History major",PROGRAM "History specialist",PROGRAM "Political Science major",PROGRAM "Political Science specialist"])
    --, ("Enrolment in ASMAJ1618. A student must be in third or fourth year.", REQAND [PROGRAM "ASMAJ1618",RAW "A student must be in third or fourth year."])
    , ("Enrolment in the PSY Research Specialist program, and PSY309H1, and one of PSY319H1/ PSY329H1/ PSY339H1", REQAND [PROGRAM "PSY Research Specialist",J "PSY309H1" "",REQOR [J "PSY319H1" "",J "PSY329H1" "",J "PSY339H1" ""]])
    , ("70% in SOC212H1 and enrolment in Sociology program", REQAND [GRADE "70" (J "SOC212H1" ""),PROGRAM "Sociology"])
    , ("(70% in SOC212H1 and enrolment in Sociology program)", REQAND [GRADE "70" (J "SOC212H1" ""),PROGRAM "Sociology"])
    , ("Admission to International Relations Major or Specialist program", REQOR [PROGRAM "International Relations Major",PROGRAM "International Relations Specialist"])
    , ("Instructor’s permission required for admission to course", RAW "Instructor\8217s permission required for admission to course")
    , ("MGT100H1, or enrolment in the Actuarial Science Specialist or Major", REQOR [J "MGT100H1" "",PROGRAM "Actuarial Science Specialist",PROGRAM "Actuarial Science Major"])
    , ("Enrolment in Psychology Minor", PROGRAM "Psychology Minor")
    , ("Enrolment in History major or permission of instructor", REQOR [PROGRAM "History major",RAW "permission of instructor"])
    , ("enrolment in a science, mathematics, or engineering program", REQOR [PROGRAM "science",PROGRAM "mathematics",PROGRAM "or engineering"])
    , ("enrolment in a science, mathematics, or engineering program, or permission from instructor", REQOR [PROGRAM "science",PROGRAM "mathematics",PROGRAM "or engineering", RAW "permission from instructor"])
    ]

cgpaInputs :: [(String, Req)]
cgpaInputs = [
      ("cGPA 1.0", GPA 1.0 "")
    , ("with 3.3 CGPA and permission of the Innis College Vice-Principal", REQAND [GPA 3.3 "",RAW "permission of the Innis College Vice-Principal"])
    , ("a minimum cGPA of 3.0, and an application form that includes a written proposal confirming a faculty member has agreed to supervise.", REQAND [GPA 3.0 "",RAW "an application form that includes a written proposal confirming a faculty member has agreed to supervise."])
    , ("and a minimum cGPA of 3.0", GPA 3.0 "")
    , ("and minimum cGPA of 3.0", GPA 3.0 "")
    , ("with a minimum cGPA of 3.0 in all CHM credits.", GPA 3.0 "in all CHM credits")
    , ("Minimum cGPA of 1.0 in CHM program courses.", GPA 1.0 "in CHM program courses")
    , ("a CGPA of at least 1.0 and permission of the Undergraduate Coordinator", REQAND [GPA 1.0 "",RAW "permission of the Undergraduate Coordinator"])
    , ("with a CGPA of at least 2.5", GPA 2.5 "")
    , ("and will normally have a CGPA of at least 3.0", GPA 3.0 "")
    , ("A minimum CGPA of 1.0, completion of 10.0 credits and permission of the College Program Director", REQAND [GPA 1.0 "",FCES 10.0 (REQUIREMENT $ RAW ""),RAW "permission of the College Program Director"])
    , ("A minimum CGPA of 3.0, completion of 12.0 credits, and permission of the College Program Director", REQAND [GPA 3.0 "",FCES 12.0 (REQUIREMENT $ RAW ""),RAW "permission of the College Program Director"])
    , ("A minimum CGPA of 2.5, completion of 15.0 credits, and an application are required", REQAND [GPA 2.5 "",FCES 15.0 (REQUIREMENT $ RAW ""),RAW "an application are required"])
    , ("A minimum CGPA of 2.0 and have completed 10.0 credits and permission of College Program Director", REQAND [GPA 2.0 "",FCES 10.0 (REQUIREMENT $ RAW ""),RAW "permission of College Program Director"])
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
orTests = createTest reqParser "Basic or Requirement" orInputs

andTests :: Test
andTests = createTest reqParser "Basic and Requirement" andInputs

andorTests :: Test
andorTests = createTest reqParser "Basic and-or-mixed Requirement" andorInputs

parTests :: Test
parTests = createTest reqParser "Basic and-or-parenthesized Requirement" parInputs

fcesTests:: Test
fcesTests = createTest reqParser "Basic fces Requirement" fcesInputs

cgpaTests :: Test
cgpaTests = createTest reqParser "Minimum cGPA Requirement" cgpaInputs

-- Outdated
-- fromParTests :: Test
-- fromParTests = createTest reqParser "Paranthesized From Requirements with integer or float fces" fromParInputs

gradeBefTests :: Test
gradeBefTests = createTest reqParser "Basic grade requirements which come before." gradeBefInputs

gradeAftTests :: Test
gradeAftTests = createTest reqParser "Basic grade requirements, where grades come after." gradeAftInputs

artSciTests :: Test
artSciTests = createTest reqParser "Arts and Science requirements from Christine's output" artSciInputs

programOrTests :: Test
programOrTests = createTest reqParser "program requirements" programOrInputs

noPrereqTests :: Test
noPrereqTests = createReqParserTest "No prerequisites required" noPrereqInputs

-- functions for running tests in REPL
reqTestSuite :: Test
reqTestSuite = TestLabel "ReqParser tests" $ TestList [orTests, andTests, andorTests, parTests, fcesTests, gradeBefTests, gradeAftTests, artSciTests, programOrTests, cgpaTests, noPrereqTests]
