{-# LANGUAGE OverloadedStrings #-}

module PostResponse where
import Data.List
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts
import SVGGen

postResponse :: ServerPart Response
postResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Check My POSt!"
                    [H.meta ! A.name "keywords"
                            ! A.content "",
                            postLinks
                    ]
                    (do header "post"
                        checkPost
                    )
                    postScripts

checkPost :: H.Html
checkPost =  do
    H.html $ do
        H.head $ do
            H.title "Check My Post!"
        H.nav ! A.id "posts" $ H.ul $ do
            H.li ! A.id "specialist" $ do
                H.a ! A.href "" $ "Specialist"
                H.div ! A.id "spec_creds" $ "(0/12.0)"
            H.li ! A.id "major" $ do
                H.a ! A.href "" $ "Major"
                H.div ! A.id "maj_creds" $ "(0/8.0)"
            H.li ! A.id "minor" $ do
                H.a ! A.href "" $ "Minor"
                H.div ! A.id "min_creds" $ "(0/4.0)"
        H.div ! A.id "div_specialist" $ do
            H.p $ do
                "CSC108H (Introduction to Computer Programming)"
            H.p $ do
                "CSC148H (Introduction to Computer Science)"
            H.p $ do
                "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                H.i " and "
                "CSC236H (Intro to the Theory of Computation)"
                H.i " or "
                "CSC240H (Enriched Intro to the Theory of Computation)"
            H.p $ do
                "MAT135H (Calculus 1(A))"
                H.i " and "
                "MAT136H (Calculus 1(B))"
                H.i " or "
                "MAT137Y (Calculus)"
                H.i " or "
                "MAT157Y (Analysis 1)"
            H.p $ do
                "CSC207H (Software Design)"
            H.p $ do
                "CSC209H (Software Tools And System Programming)"
            H.p $ do
                "CSC258H (Computer Organization)"
            H.p $ do
                "CSC263H (Data Structures and Analysis)"
                H.i " or "
                "CSC265H (Enriched Data Structures and Analysis)"
            H.p $ do
                "MAT221H (Applied Linear Algebra)"
                H.i " or "
                "MAT223H (Linear Algebra 1)"
            H.p $ do
                "STA247H (Probability with Computer Applications)"
                H.i " or "
                "STA255H (Statistical Analysis)"
                H.i " or "
                "STA257H (Probability and Statistics 1)"
            H.p $ do
                "CSC369H (Operating Systems)"
            H.p $ do
                "CSC373H (Algorithm Design, Analysis, and Complexity)"
            H.p $ do
                "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y (1.5 FCEs)"
            H.p $ do
                "Any 300-level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)"
            H.p $ H.em "Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H (2.0 FCEs)"
            H.p $ H.em "No more than 1.0 FCEs from CSC490H, CSC491H, CSC494H, CSC495H, BCB430Y"
        H.div ! A.id "div_major" $ do
            H.p $ do
                "CSC108H (Introduction to Computer Programming)"
            H.p $ do
                "CSC148H (Introduction to Computer Science)"
            H.p $ do
                "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                H.i " and "
                "CSC236H (Intro to the Theory of Computation)"
                H.i " or "
                "CSC240H (Enriched Intro to the Theory of Computation)"
            H.p $ do
                "MAT135H (Calculus 1(A))"
                H.i " and "
                "MAT136H (Calculus 1(B))"
                H.i " or "
                "MAT137Y (Calculus)"
                H.i " or "
                "MAT157Y (Analysis 1)"
            H.p $ do
                "CSC207H (Software Design)"
            H.p $ do
                "CSC258H (Computer Organization)"
            H.p $ do
                "CSC263H (Data Structures and Analysis)"
                H.i " or "
                "CSC265H (Enriched Data Structures and Analysis)"
            H.p $ do
                "STA247H (Probability with Computer Applications)"
                H.i " or "
                "STA255H (Statistical Analysis)"
                H.i " or "
                "STA257H (Probability and Statistics 1)"
            H.p $ do
                "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y (1.5 FCEs)"
            H.p $ do
                "Any 300-level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H, (1.5 FCEs)"
            H.p $ H.em "Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H (2.0 FCEs)"
            H.p $ H.em "No more than 1.0 FCEs from CSC490H, CSC491H, CSC494H, CSC495H, BCB430Y"
        H.div ! A.id "div_minor" $ do
            H.p $ do
                "CSC108H (Introduction to Computer Programming)"
            H.p $ do
                "CSC148H (Introduction to Computer Science)"
            H.p $ do
                "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                H.i " and "
                "CSC236H (Intro to the Theory of Computation)"
                H.i " or "
                "CSC240H (Enriched Intro to the Theory of Computation)"
            H.p $ do
                "Any 300/400-level CSC course (atleast 1.0 FCE), CSC209H, CSC258H, CSC263H/CSC265H (1.5 FCEs)"

                   