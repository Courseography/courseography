{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module RequirementTests.ReqParserTests
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
      ("CSC120H1/CSC148H1", ReqOr [J "CSC120H1" "", J "CSC148H1" ""])
    , ("CSC108H1/CSC120H1/CSC148H1", ReqOr [J "CSC108H1" "", J "CSC120H1" "", J "CSC148H1" ""])
    , ("FOR300H1/FOR301H1/FOR302H1", ReqOr [J "FOR300H1" "", J "FOR301H1" "", J "FOR302H1" ""])
    , ("BIO130H1 or BIO206H5 or BIOB10H3", ReqOr [J "BIO130H1" "", J "BIO206H5" "", J "BIOB10H3" ""])
    , ("CHM220H1 with a minimum grade of B, or CHM222H1", ReqOr [Grade "B" (J "CHM220H1" ""),J "CHM222H1" ""])
    ]

andInputs :: [(String, Req)]
andInputs = [
      ("CSC165H1, CSC236H1", ReqAnd [J "CSC165H1" "", J "CSC236H1" ""])
    , ("CSC120H1, CSC121H1, CSC148H1", ReqAnd [J "CSC120H1" "", J "CSC121H1" "", J "CSC148H1" ""])
    , ("CSC165H1 & CSC236H1", ReqAnd [J "CSC165H1" "", J "CSC236H1" ""])
    , ("BCH377H1; BCH378H1; and permission of Department", ReqAnd [J "BCH377H1" "", J "BCH378H1" "", Raw "permission of Department"])
    , ("CSC111H1. Permission of department", ReqAnd [J "CSC111H1" "", Raw "Permission of department"])
    , ("CSC110H1, an additional permission of department", ReqAnd [J "CSC110H1" "", Raw "permission of department"])
    , ("CSC110H1, additional 2.0 credits from CSC", ReqAnd [J "CSC110H1" "", Fces 2.0 (Department "CSC")])
    ]

andorInputs :: [(String, Req)]
andorInputs = [
      ("CSC148H1/CSC207H1, CSC165H1/CSC236H1", ReqAnd [ReqOr [J "CSC148H1" "", J "CSC207H1" ""], ReqOr [J "CSC165H1" "", J "CSC236H1" ""]])
    , ("COG250Y1 and one of: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", ReqAnd [J "COG250Y1" "", ReqOr [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 and one of either LIN232H1/LIN241H1 or JLP315H1/JLP374H1", ReqAnd [J "COG250Y1" "", ReqOr [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("COG250Y1 + one of the following: LIN232H1/LIN241H1 or JLP315H1/JLP374H1", ReqAnd [J "COG250Y1" "", ReqOr [J "LIN232H1" "", J "LIN241H1" "", J "JLP315H1" "", J "JLP374H1" ""]])
    , ("CLA204H1 + 1 OF CLA160H1/CLA260H1", ReqAnd [J "CLA204H1" "", ReqOr [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("BIO220H1 and at least one of EEB319H1/EEB321H1", ReqAnd [J "BIO220H1" "", ReqOr [J "EEB319H1" "", J "EEB321H1" ""]])
    , ("CLA204H1 plus one of CLA160H1/CLA260H1", ReqAnd [J "CLA204H1" "", ReqOr [J "CLA160H1" "", J "CLA260H1" ""]])
    , ("ARH205H1/ ARH305H1, and one of ANT100Y1/ ANT200Y1/ ANT356H1", ReqAnd [ReqOr [J "ARH205H1" "",J "ARH305H1" ""],ReqOr [J "ANT100Y1" "",J "ANT200Y1" "",J "ANT356H1" ""]])
    ]

parInputs :: [(String, Req)]
parInputs = [
      ("(CSC148H1)", J "CSC148H1" "")
    , ("CSC108H1, (CSC165H1/CSC148H1)", ReqAnd [J "CSC108H1" "", ReqOr [J "CSC165H1" "", J "CSC148H1" ""]])
    , ("(MAT135H1, MAT136H1)/ MAT137Y1", ReqOr [ReqAnd [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" ""])
    , ("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", ReqOr [J "CSC148H1" "", ReqAnd [ReqOr [J "CSC108H1" "", J "CSC120H1" ""], ReqOr [J "MAT137Y1" "", J "MAT157Y1" ""]]])
    , ("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", ReqAnd [ReqOr [J "STA247H1" "", J "STA255H1" "", J "STA257H1" "", J "PSY201H1" "", J "ECO227Y1" ""], ReqOr [ReqAnd [J "MAT135H1" "", J "MAT136H1" ""], J "MAT137Y1" "", J "MAT157Y1" ""]])
    ]


fcesInputs :: [(String, Req)]
fcesInputs = [
      ("1.0 FCE from the following: (CSC148H1)", Fces 1.0 $ Requirement $ J "CSC148H1" "")
    , ("2.0 FCEs from CSC165H1/CSC148H1", Fces 2.0 $ Requirement $ ReqOr [J "CSC165H1" "", J "CSC148H1" ""])
    , ("2.0 FCEs in CSC165H1/CSC148H1", Fces 2.0 $ Requirement $ ReqOr [J "CSC165H1" "", J "CSC148H1" ""])
    , ("2 FCEs from: MAT135H1, MAT136H1/ MAT137Y1", Fces 2.0 $ Requirement $ ReqAnd [J "MAT135H1" "",ReqOr [J "MAT136H1" "",J "MAT137Y1" ""]])
    , ("Completion of 4.0 FCEs", Fces 4.0 $ Requirement $ Raw "")
    , ("Completion of 4 FCE.", Fces 4.0 $ Requirement $ Raw "")
    , ("Completion of 9 FCEs", Fces 9.0 $ Requirement $ Raw "")
    , ("Completion of 9.0 credits or permission of the instructor", ReqOr [Fces 9.0 (Requirement $ Raw ""), Raw "permission of the instructor"])
    , ("Completion of 9.0 credits. Permission of the instructor", ReqAnd [Fces 9.0 (Requirement $ Raw ""), Raw "Permission of the instructor"])
    , ("Completion of at least 9.0 FCE", Fces 9.0 $ Requirement $ Raw "")
    , ("Completion of a minimum of 4.0 FCEs", Fces 4.0 $ Requirement $ Raw "")
    , ("Completion of a minimum of 9 FCEs", Fces 9.0 $ Requirement $ Raw "")
    , ("Completion of 4.0 credits", Fces 4.0 $ Requirement $ Raw "")
    , ("at least 4.0 credits", Fces 4.0 $ Requirement $ Raw "")
    , ("At least one 0.5 credit at the 400-level", Fces 0.5 (Level "400"))
    , ("At least 1.5 credits at the 400-level", Fces 1.5 (Level "400"))
    , ("2.0 credits from 300 level CSC courses", Fces 2.0 (ModAnd [Level "300", Department "CSC"]))
    , ("1.0 credits at the 300-level or higher", Fces 1.0 (Level "300+"))
    , ("1.0 credits at the 300+ level", Fces 1.0 (Level "300+"))
    , ("1.0 credits of 300+ level CSC courses", Fces 1.0 (ModAnd [Level "300+", Department "CSC"]))
    , ("1.0 credits at the 300-level or higher from CSC courses", Fces 1.0 (ModAnd [Level "300+", Department "CSC"]))
    , ("1.0 credit at the 300-level from Group B: Evolutionary", Fces 1.0 (ModAnd [Level "300", Department "Group B: Evolutionary"]))
    , ("ANT203Y1 and a 0.5 credit 300+ level course from Group B: Evolutionary", ReqAnd [J "ANT203Y1" "", Fces 0.5 (ModAnd [Level "300+", Department "Group B: Evolutionary"])])
    , ("ANT253H1 and 1.0 credit at the 300-level", ReqAnd [J "ANT253H1" "", Fces 1.0 (Level "300")])
    , ("1.0 credit in FRE at the 200-level", Fces 1.0 (ModAnd [Department "FRE", Level "200"]))
    , ("1.0 CLA credit at the 300-level", Fces 1.0 (ModAnd [Level "300", Department "CLA"]))
    , ("14.0 credits, 3.0 credits in Anthropology", ReqAnd [Fces 14.0 (Requirement $ Raw ""), Fces 3.0 (Department "Anthropology")])
    , ("Completion of 14.0 credits including PSL300H1, PSL301H1, and 0.5 HMB credit at the 300-level", ReqAnd [Fces 14.0 (Requirement $ Raw ""), J "PSL300H1" "", J "PSL301H1" "", Fces 0.5 $ ModAnd [Level "300", Department "HMB"]])
    , ("1.0 credits, CSE240H1/ NEW240Y1, CSE342H1, an additional 0.5 credit at the 300+ level from the Critical Studies", ReqAnd [Fces 1.0 (Requirement $ Raw ""),ReqOr [J "CSE240H1" "",J "NEW240Y1" ""],J "CSE342H1" "", Fces 0.5 (ModAnd [Level "300+", Department "Critical Studies"])])
    , ("1.0 credits or 1.0 credit in Canadian Studies", ReqOr [Fces 1.0 (Requirement $ Raw ""),Fces 1.0 (Department "Canadian Studies")])
    , ("NEW240Y1, an additional 0.5 credits at the 300 level from the Critical Studies", ReqAnd [J "NEW240Y1" "", Fces 0.5 (ModAnd [Level "300", Department "Critical Studies"])])
    , ("At least one 0.5 credit at the 400-level or permission of the instructor", ReqOr [Fces 0.5 (Level "400"), Raw "permission of the instructor"])
    , ("0.5 credit in HPS", Fces 0.5 (Department "HPS"))
    , ("1.0 credit in MST courses and 0.5 credit in HIS", ReqAnd [Fces 1.0 (Department "MST"), Fces 0.5 (Department "HIS")])
    , ("2.0 ENG credits", Fces 2.0 (Department "ENG"))
    , ("Any 9.0 credits", Fces 9.0 (Requirement $ Raw ""))
    , ("2.0 ENG credits and any 4.0 credits", ReqAnd [Fces 2.0 (Department "ENG"), Fces 4.0 (Requirement $ Raw "")])
    , ("9.0 credits in any field", Fces 9.0 (Requirement $ Raw ""))
    , ("9.0 credits in any subject", Fces 9.0 (Requirement $ Raw ""))
    , ("1.0 credits of CSC courses", Fces 1.0 (Department "CSC"))
    , ("1.0 credits from the CSC courses", Fces 1.0 (Department "CSC"))
    , ("1.0 credits at the 400-level", Fces 1.0 (Level "400"))
    , ("a 0.5 credit 300 level course", Fces 0.5 (Level "300"))
    , ("at least one additional 0.5 credit from a 300-/400-level course", Fces 0.5 (ModOr [Level "300", Level "400"]))
    , ("at least one additional 0.5 credit from a 400-level CSC/BCB course", Fces 0.5 (ModAnd [Level "400", ModOr [Department "CSC", Department "BCB"]]))
    , ("at least one additional 1.0 credit from a 300-/400-level CSC/BCB courses", Fces 1.0 (ModAnd [ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"]]))
    , ("At least 1.0 credit must be at the 300-/400-level.", Fces 1.0 (ModOr [Level "300", Level "400"]))
    , ("At least 1.5 credit must be at the 400-level CSC or BCB courses.", Fces 1.5 (ModAnd [Level "400", ModOr [Department "CSC", Department "BCB"]]))
    , ("At least 1.5 CSC/BCB credits at the 400-level", Fces 1.5 (ModAnd [Level "400", ModOr [Department "CSC", Department "BCB"]]))
    , ("At least 1.5 CSC/BCB credits at the 300-/400-level", Fces 1.5 (ModAnd [ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"]]))
    , ("At least 1.5 Psychology/Canadian Studies or Group B: Evolutionary credits at the 300-/400-level", Fces 1.5 (ModAnd [ModOr [Level "300", Level "400"], ModOr [Department "Psychology", Department "Canadian Studies", Department "Group B: Evolutionary"]]))
    ]

gradeBefInputs :: [(String, Req)]
gradeBefInputs = [
      ("minimum mark of A- in CSC236H1", Grade "A-" $ J "CSC236H1" "")
    , ("minimum grade of 75% CSC236H1", Grade "75" $ J "CSC236H1" "")
    , ("minimum of 75% CSC236H1", Grade "75" $ J "CSC236H1" "")
    , ("minimum (75%) CSC236H1", Grade "75" $ J "CSC236H1" "")
    , ("A grade of 75% in CSC236H1", Grade "75" $ J "CSC236H1" "")
    , ("At least C+ in CSC236H1", Grade "C+" $ J "CSC236H1" "")
    , ("A C+ in CSC236H1", Grade "C+" $ J "CSC236H1" "")
    , ("Minimum of 75% CSC236H1", Grade "75" $ J "CSC236H1" "")
    , ("Grade of C+ in CSC236H1", Grade "C+" $ J "CSC236H1" "")
    , ("A final grade of C+ in CSC236H1", Grade "C+" $ J "CSC236H1" "")
    , ("75% in CSC236H1", Grade "75" $ J "CSC236H1" "")
    ]

gradeAftInputs :: [(String, Req)]
gradeAftInputs = [
      ("CSC236H1 75%", Grade "75" $ J "CSC236H1" "")
    , ("CSC236H1 (75%)", Grade "75" $ J "CSC236H1" "")
    , ("CSC236H1(75%)", Grade "75" $ J "CSC236H1" "")
    , ("CSC263H1 (C+)", Grade "C+" $ J "CSC263H1" "")
    , ("CSC263H1 B-", Grade "B-" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum grade of 60%", Grade "60" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum mark of B-", Grade "B-" $ J "CSC263H1" "")
    , ("CSC236H1 (at least 75% or more)", Grade "75" $ J "CSC236H1" "")
    , ("CSC236H1 ( 75% or higher )", Grade "75" $ J "CSC236H1" "")
    , ("CSC263H1 with a minimum grade of 60% or more", Grade "60" $ J "CSC263H1" "")
    , ("CSC263H1 with a minimum grade of 60% or higher / CSC236H1 (75%)", ReqOr [Grade "60" $ J "CSC263H1" "", Grade "75" $ J "CSC236H1" ""])
    , ("A in CSC236H1", Grade "A" $ J "CSC236H1" "")
    ]

artSciInputs :: [(String, Req)]
artSciInputs = [
      ("BIO220H1 (ecology and evolutionary biology)", J "BIO220H1" "ecology and evolutionary biology")
    , ("EEB223H1/ STA220H1 (recommended)/ STA257H1 (recommended)", ReqOr [J "EEB223H1" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    , ("EEB223H1 (ecology and evo), STA220H1 (recommended)/ STA257H1 (recommended)", ReqAnd [J "EEB223H1" "ecology and evo",ReqOr [J "STA220H1" "recommended",J "STA257H1" "recommended"]])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (recommended)/ STA257H1", ReqOr [J "EEB223H1" "ecology and evo",J "STA220H1" "recommended",J "STA257H1" ""])
    , ("EEB223H1 (ecology and evo)/ STA220H1 (B-)/ STA257H1", ReqOr [J "EEB223H1" "ecology and evo", Grade "B-" $ J "STA220H1" "", J "STA257H1" ""])
    , ("0.5 FCE from: EEB225H1 (recommended)/ STA220H1 (B-)/ STA257H1/  STA288H1/ GGR270H1/ PSY201H1", Fces 0.5 $ Requirement $ ReqOr [J "EEB225H1" "recommended", Grade "B-" $ J "STA220H1" "", J "STA257H1" "", J "STA288H1" "", J "GGR270H1" "", J "PSY201H1" ""])
    , ("MATB23H3/STA220H1 (recommended)/STA257H1 (recommended)", ReqOr [J "MATB23H3" "",J "STA220H1" "recommended",J "STA257H1" "recommended"])
    ]

programOrInputs :: [(String, Req)]
programOrInputs = [
      ("Admission to Vic One", Program "Vic One")
    , ("Enrolment in the International Relations program or in a History major or specialist program, or permission of instructor", ReqOr [Program "International Relations",Program "History major",Program "History specialist", Raw "permission of instructor"])
    , ("Enrolment in the International Relations program or in a History or Political Science major or specialist program", ReqOr [Program "International Relations",Program "History major",Program "History specialist",Program "Political Science major",Program "Political Science specialist"])
    --, ("Enrolment in ASMAJ1618. A student must be in third or fourth year.", ReqAnd [Program "ASMAJ1618",Raw "A student must be in third or fourth year."])
    , ("Enrolment in the PSY Research Specialist program, and PSY309H1, and one of PSY319H1/ PSY329H1/ PSY339H1", ReqAnd [Program "PSY Research Specialist",J "PSY309H1" "",ReqOr [J "PSY319H1" "",J "PSY329H1" "",J "PSY339H1" ""]])
    , ("70% in SOC212H1 and enrolment in Sociology program", ReqAnd [Grade "70" (J "SOC212H1" ""),Program "Sociology"])
    , ("(70% in SOC212H1 and enrolment in Sociology program)", ReqAnd [Grade "70" (J "SOC212H1" ""),Program "Sociology"])
    , ("Admission to International Relations Major or Specialist program", ReqOr [Program "International Relations Major",Program "International Relations Specialist"])
    , ("Instructor’s permission required for admission to course", Raw "Instructor\8217s permission required for admission to course")
    , ("MGT100H1, or enrolment in the Actuarial Science Specialist or Major", ReqOr [J "MGT100H1" "",Program "Actuarial Science Specialist",Program "Actuarial Science Major"])
    , ("Enrolment in Psychology Minor", Program "Psychology Minor")
    , ("Enrolment in History major or permission of instructor", ReqOr [Program "History major",Raw "permission of instructor"])
    , ("enrolment in a science, mathematics, or engineering program", ReqOr [Program "science",Program "mathematics",Program "or engineering"])
    , ("enrolment in a science, mathematics, or engineering program, or permission from instructor", ReqOr [Program "science",Program "mathematics",Program "or engineering", Raw "permission from instructor"])
    ]

cgpaInputs :: [(String, Req)]
cgpaInputs = [
      ("cGpa 1.0", Gpa 1.0 "")
    , ("with 3.3 CGpa and permission of the Innis College Vice-Principal", ReqAnd [Gpa 3.3 "",Raw "permission of the Innis College Vice-Principal"])
    , ("a minimum cGpa of 3.0, and an application form that includes a written proposal confirming a faculty member has agreed to supervise.", ReqAnd [Gpa 3.0 "",Raw "an application form that includes a written proposal confirming a faculty member has agreed to supervise."])
    , ("and a minimum cGpa of 3.0", Gpa 3.0 "")
    , ("and minimum cGpa of 3.0", Gpa 3.0 "")
    , ("with a minimum cGpa of 3.0 in all CHM credits.", Gpa 3.0 "in all CHM credits")
    , ("Minimum cGpa of 1.0 in CHM program courses.", Gpa 1.0 "in CHM program courses")
    , ("a CGpa of at least 1.0 and permission of the Undergraduate Coordinator", ReqAnd [Gpa 1.0 "",Raw "permission of the Undergraduate Coordinator"])
    , ("with a CGpa of at least 2.5", Gpa 2.5 "")
    , ("and will normally have a CGpa of at least 3.0", Gpa 3.0 "")
    , ("A minimum CGpa of 1.0, completion of 10.0 credits and permission of the College Program Director", ReqAnd [Gpa 1.0 "",Fces 10.0 (Requirement $ Raw ""),Raw "permission of the College Program Director"])
    , ("A minimum CGpa of 3.0, completion of 12.0 credits, and permission of the College Program Director", ReqAnd [Gpa 3.0 "",Fces 12.0 (Requirement $ Raw ""),Raw "permission of the College Program Director"])
    , ("A minimum CGpa of 2.5, completion of 15.0 credits, and an application are required", ReqAnd [Gpa 2.5 "",Fces 15.0 (Requirement $ Raw ""),Raw "an application are required"])
    , ("A minimum CGpa of 2.0 and have completed 10.0 credits and permission of College Program Director", ReqAnd [Gpa 2.0 "",Fces 10.0 (Requirement $ Raw ""),Raw "permission of College Program Director"])
    ]

noPrereqInputs :: [(String, Req)]
noPrereqInputs = [
      ("", None)
    , ("None", None)
    , ("none", None)
    , ("No", None)
    , ("no", None)
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
cgpaTests = createTest reqParser "Minimum cGpa Requirement" cgpaInputs

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
