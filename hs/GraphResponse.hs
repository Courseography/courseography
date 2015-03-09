{-# LANGUAGE OverloadedStrings #-}

module GraphResponse where

import Data.List
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Graph"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 plannerLinks
                ]
                (do header "graph"
                    H.div ! A.id "container" $ do
                        H.div ! A.id "graph" ! A.class_ "graph" $   ""
                        sideBar
                    disclaimer
                )
                plannerScripts


fceCountDiv :: H.Html
fceCountDiv = 
    createTag H.div "FCECountDiv" "" $ do
        createTag H.span "FCEcount" "" "0.0"
        "FCEs" -- Being difficult. Won't show up correctly.

focusesTab :: H.Html
focusesTab = ""

timetableTab :: H.Html
timetableTab = 
    createTag H.div "timetable" "infoTab" $ do
        createTag H.div "timetableSearch" "infoTabContent" $ do
            createTag H.h2 "" "" "2014-2015 Timetable"
            H.p "Search through the timetable for a course or instructor."
            H.p "The \"(+5)\" caps are extra reserved seats. See official timetable for details."
            createTag H.p "timetable-creator-link" "" $
                makeA "" "" "timetable_creator.html" "" "Plan your timetable here!"
            makeInput "filter" "text-input" "" "" "text"
        createTag H.div "timetableContainer" "" ""

sideBar :: H.Html
sideBar = do
    H.div ! A.id "sidebar" $ do
        H.div ! A.id "fcecount" $ ""
        H.nav ! A.id "sidebar-nav" $ H.ul $ do
            H.li ! A.id "focuses-nav" $ do
                H.a ! A.href "" $ "Focuses"
            H.li ! A.id "graphs-nav" $ do
                H.a ! A.href "" $ "Graphs"
        H.div ! A.id "focuses" $ do
            H.p ! A.id "scientific-computing" ! A.class_ "focus" $ "Scientific Computing"
            H.p ! A.id "ai" ! A.class_ "focus" $ "Artifical Intelligence"
            H.p ! A.id "natural-language" ! A.class_ "focus" $ "Natural Language Processing"
            H.p ! A.id "computer-vision" ! A.class_ "focus" $ "Computer Vision"
            H.p ! A.id "computer-systems" ! A.class_ "focus" $ "Computer Systems"
            H.p ! A.id "video-games" ! A.class_ "focus" $ "Video Games"
            H.p ! A.id "hci" ! A.class_ "focus" $ "Human Computer Interaction"
            H.p ! A.id "theory-comp" ! A.class_ "focus" $ "Theory of Computation"
            H.p ! A.id "web" ! A.class_ "focus" $ "Web Technologies"
        H.div ! A.id "graphs" $ "Graph"
    H.div ! A.id "sidebar-button" $ ""




