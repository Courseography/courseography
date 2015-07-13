{-# LANGUAGE OverloadedStrings, DataKinds #-}

module PostResponse
    (postResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MasterTemplate
import Scripts

postResponse :: ServerPart Response
postResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Check My POSt!"
                    []
                    (do
                        header "post"
                        checkPost
                    )
                    postScripts

checkPost :: H.Html
checkPost =
    H.html $ do
        H.head $
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
            H.h2 "First Year"
            H.div ! A.id "spec_csc108" $ do
                H.p ! A.class_ "code" $ "CSC108H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC108" $ "CSC108H (Introduction to Computer Programming)"
            H.div ! A.id "spec_csc148" $ do
                H.p ! A.class_ "code" $ "CSC148H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC148" $ "CSC148H (Introduction to Computer Science)"
            H.div ! A.id "spec_csc165240" $ do
                H.p ! A.class_ "code" $ "CSC165H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC165240" $ "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                    H.p ! A.class_ "full_name CSC165240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.div ! A.id "spec_mat135136137157calc1" $ do
                H.p ! A.class_ "code" $ "(MAT135H and MAT136H) or MAT137Y or MAT157Y"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT135H (Calculus 1(A))"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT136H (Calculus 1(B))"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT137Y (Calculus)"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT157Y (Analysis 1)"
            H.h2 "Second Year"
            H.div ! A.id "spec_csc207" $ do
                H.p ! A.class_ "code" $ "CSC207H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC207" $ "CSC207H (Software Design)"
            H.div ! A.id "spec_csc209" $ do
                H.p ! A.class_ "code" $ "CSC209H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC209" $ "CSC209H (Software Tools And System Programming)"
            H.div ! A.id "spec_csc258" $ do
                H.p ! A.class_ "code" $ "CSC258H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC258" $ "CSC258H (Computer Organization)"
            H.div ! A.id "spec_csc236240" $ do
                H.p ! A.class_ "code" $ "CSC236H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC236240" $ "CSC236H (Introduction to the Theory of Computation)"
                    H.p ! A.class_ "full_name CSC236240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.div ! A.id "spec_csc263265" $ do
                H.p ! A.class_ "code" $ "CSC263H or CSC265H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC263265" $ "CSC263H (Data Structures and Analysis)"
                    H.p ! A.class_ "full_name CSC263265" $ "CSC265H (Enriched Data Structures and Analysis)"
            H.div ! A.id "spec_mat221223240lin1" $ do
                H.p ! A.class_ "code" $ "MAT221H or MAT223H or MAT240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name MAT221223240Lin1" $ "MAT221H (Applied Linear Algebra)"
                    H.p ! A.class_ "full_name MAT221223240Lin1" $ "MAT223H (Linear Algebra 1)"
                    H.p ! A.class_ "full_name MAT221223240Lin1" $ "MAT240H (Algebra 1)"
            H.div ! A.id "spec_sta247255257sta1" $ do
                H.p ! A.class_ "code" $ "STA247H or STA255H or STA257H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA247H (Probability with Computer Applications)"
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA255H (Statistical Analysis)"
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA257H (Probability and Statistics 1)"
            H.h2 "Later Years"
            H.div ! A.id "spec_csc369" $ do
                H.p ! A.class_ "code" $ "CSC369H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC369" $ "CSC369H (Operating Systems)"
            H.div ! A.id "spec_csc373" $ do
                H.p ! A.class_ "code" $ "CSC373H"
                H.div ! A.class_ "more-info" $
                        H.p ! A.class_ "full_name CSC373" $ "CSC373H (Algorithm Design, Analysis, and Complexity)"
            H.div ! A.id "spec_400" $ do
                H.p ! A.class_ "code" $ "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)"
                H.div ! A.id "spec400" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.div ! A.id "spec_300" $ do
                H.p ! A.class_ "code" $ "Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)"
                H.div ! A.id "spec300" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.div ! A.id "spec_extra" $ do
                H.p ! A.class_ "code" $ "Any of the following: 300+ level CSC course; MAT: 221/223/240, 235/237/257, any 300+\
                                        \ except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H;\
                                        \ BCB: 410H/420H/430Y (1.5 FCEs)"
                H.div ! A.id "specextra" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.div ! A.id "spec_misc" $ do
                H.p ! A.class_ "code" $ "Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, \
                    \CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) \
                    \ ** Note: Type 'PEY' for Check my POSt to recognize it **"
                H.div ! A.class_ "more-info" $
                    H.input ! A.type_ "text"
            H.h3 "Notes"
            H.div ! A.id "notes" $
                H.p "- No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used to fulfill program requirements"
        H.div ! A.id "div_major" $ do
            H.h2 "First Year"
            H.div ! A.id "maj_csc108" $ do
                H.p ! A.class_ "code" $ "CSC108H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC108" $ "CSC108H (Introduction to Computer Programming)"
            H.div ! A.id "maj_csc148" $ do
                H.p ! A.class_ "code" $ "CSC148H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC148" $ "CSC148H (Introduction to Computer Science)"
            H.div ! A.id "maj_csc165240" $ do
                H.p ! A.class_ "code" $ "CSC165H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC165240" $ "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                    H.p ! A.class_ "full_name CSC165240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.div ! A.id "maj_mat135136137157calc1" $ do
                H.p ! A.class_ "code" $ "(MAT135H and MAT136H) or MAT137Y or MAT157Y"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT135H (Calculus 1(A))"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT136H (Calculus 1(B))"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT137Y (Calculus)"
                    H.p ! A.class_ "full_name MAT135136137157Calc1" $ "MAT157Y (Analysis 1)"
            H.h2 "Second Year"
            H.div ! A.id "maj_csc207" $ do
                H.p ! A.class_ "code" $ "CSC207H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC207" $ "CSC207H (Software Design)"
            H.div ! A.id "maj_csc258" $ do
                H.p ! A.class_ "code" $ "CSC258H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC258" $ "CSC258H (Computer Organization)"
            H.div ! A.id "maj_csc236240" $ do
                H.p ! A.class_ "code" $ "CSC236H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC236240" $ "CSC236H (Introduction to the Theory of Computation)"
                    H.p ! A.class_ "full_name CSC236240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.div ! A.id "maj_csc263265" $ do
                H.p ! A.class_ "code" $ "CSC263H or CSC265H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC263265" $ "CSC263H (Data Structures and Analysis)"
                    H.p ! A.class_ "full_name CSC263265" $ "CSC265H (Enriched Data Structures and Analysis)"
            H.div ! A.id "maj_sta247255257sta1" $ do
                H.p ! A.class_ "code" $ "STA247H or STA255H or STA257H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA247H (Probability with Computer Applications)"
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA255H (Statistical Analysis)"
                    H.p ! A.class_ "full_name STA247255257Sta1" $ "STA257H (Probability and Statistics 1)"
            H.h2 "Later Years"
            H.div ! A.id "maj_400" $ do
                H.p ! A.class_ "code" $ "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y (0.5 FCEs)"
                H.div ! A.id "maj400" ! A.class_ "more-info" $
                    H.input ! A.type_ "text"
            H.div ! A.id "maj_300" $ do
                H.p ! A.class_ "code" $ "Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.0 FCEs)"
                H.div ! A.id "maj300" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.div ! A.id "maj_extra" $ do
                H.p ! A.class_ "code" $ "Any of the following: 200+ level CSC course; MAT: 221/223/240, 235/237/257,\
                    \ any 300+ except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H;\
                    \ BCB: 410H/420H/430Y (1.5 FCEs, with at least 0.5 FCEs in the 300+ level)"
                H.div ! A.id "majextra" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.div ! A.id "maj_misc" $ do
                H.p ! A.class_ "code" $ "Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, \
                    \CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) \
                    \ ** Note: Type 'PEY' for Check my POSt to recognize it **"
                H.div ! A.class_ "more-info" $
                    H.input ! A.type_ "text"
            H.h3 "Notes"
            H.div ! A.id "notes" $
                H.p "- No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used to fulfill program requirements"
        H.div ! A.id "div_minor" $ do
            H.h2 "First Year"
            H.div ! A.id "min_csc108" $ do
                H.p ! A.class_ "code" $ "CSC108H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC108" $ "CSC108H (Introduction to Computer Programming)"
            H.div ! A.id "min_csc148" $ do
                H.p ! A.class_ "code" $ "CSC148H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC148" $ "CSC148H (Introduction to Computer Science)"
            H.div ! A.id "min_csc165240" $ do
                H.p ! A.class_ "code" $ "CSC165H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC165240" $ "CSC165H (Mathematical Expression and Reasoning for Computer Science)"
                    H.p ! A.class_ "full_name CSC165240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.h2 "Later Years"
            H.div ! A.id "min_csc207" $ do
                H.p ! A.class_ "code" $ "CSC207H"
                H.div ! A.class_ "more-info" $
                    H.p ! A.class_ "full_name CSC207" $ "CSC207H (Software Design)"
            H.div ! A.id "min_csc236240" $ do
                H.p ! A.class_ "code" $ "CSC236H or CSC240H"
                H.div ! A.class_ "more-info" $ do
                    H.p ! A.class_ "full_name CSC236240" $ "CSC236H (Introduction to the Theory of Computation)"
                    H.p ! A.class_ "full_name CSC236240" $ "CSC240H (Enriched Introduction to the Theory of Computation)"
            H.div ! A.id "min_misc" $ do
                H.p ! A.class_ "code" $ "200+ CSC courses (1.5 FCEs, with at least 1.0 FCE in the 300+ levels)"
                H.div ! A.id "minextra" ! A.class_ "more-info" $ do
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
                    H.input ! A.type_ "text"
            H.h3 "Notes"
            H.div ! A.id "notes" $
                H.p "- You may take no more than three 300+ CSC/ECE courses"
