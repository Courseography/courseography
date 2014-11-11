{-# LANGUAGE OverloadedStrings #-}

module AboutResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate

aboutResponse :: ServerPart Response
aboutResponse =
   ok $ toResponse $
    masterTemplate "Courseography - SVG serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do 
                    header "about"
                    aboutHtml
                )
                ""


aboutHtml :: H.Html
aboutHtml = H.div ! A.id "aboutDiv" $ do
  H.h1 "About Courseography"
  H.p $ do
    "Here are the University of Toronto, we have hundreds of courses to choose from, "
    "and it can be hard navigate prerequisite chains, program requirements, "
    "and term-by-term offerings all at once. "
    "That's where Courseography comes in: by presenting course and scheduling "
    "information in a set of graphical interactive tools, we make it easier to "
    "choose the right courses for your academic career. Whether it's making sure "
    "you'll satisfy all the prerequities for that 4th year course you really want to take, "
    "or fitting together fragments of your schedule for next term, "
    "we hope Courseography makes your life easier!"
  H.p $ do
    "Courseography was envisioned in late 2013 by "
    H.a ! A.href "http://www.cs.toronto.edu/~liudavid/" $ "David Liu"
    ", then a PhD student in the Department of Computer Science. "
    "However, it wasn't until he recruited Ian Stewart-Binks to "
    "the project that things really got rolling. Though the past year "
    "has seen really seen our tools take off within the CS student body, "
    "there's still a long way for us to go. Check out our "
    H.a ! A.href "projects" $ "Projects"
    " page to find out more about our plans for expanding to new "
    H.strong "departments, faculties, and campuses"
    ", and adding new services like "
    H.strong "POSt checking"
    " and "
    H.strong "importing/exporting data"
    "."
  H.h2 "Get Involved"
  H.p $ do
    "One of our main goals for Courseography is to keep it a "
    H.strong "student-driven project"
    ". This means that if you want to get involved, you can! "
    "The best way to get started is by "
    H.a ! A.href "" $ "shooting us an email"
    ", but you should also feel free to check out all of the source code"
    " on "
    H.a ! A.href "" $ "Github"
    " and the accompanying "
    H.a ! A.href "projects" $ "Projects"
    " page. For CS students: David is also accepting "
    H.strong "CSC494/495"
    " project students for the upcoming Winter 2015 term."
  H.p $ do
    "If you're coming from outside of the Department of Computer Science, "
    "we'd love to hear from you! We welcome help on making new graphs, "
    "understanding department- or program-specific policies, "
    "pointing out bugs or incorrect information, "
    "and general thoughts on the design of our tools. "
    H.strong "You don't need to be a programmer to get involved."
  H.p $ do
    "We can be reached at "
    H.a ! A.href "mailto:cs.toronto.courseplanner@gmail.com" ! A.target "_blank"
        $ "cs.toronto.courseplanner@gmail.com"
    ", and look forward to hearing from you!"
