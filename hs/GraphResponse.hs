{-# LANGUAGE OverloadedStrings #-}

module GraphResponse where

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
                        H.div ! A.id "graph" ! A.class_ "graph" $ ""
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
        H.div ! A.id "fce" $ do
            H.div ! A.id "fcecount" $ ""
            H.button ! A.id "reset" $ "Reset Graphs"
        H.nav ! A.id "sidebar-nav" $ H.ul $ do
            H.li ! A.id "focuses-nav" $ do
                H.a ! A.href "" $ "Focuses"
            H.li ! A.id "graphs-nav" $ do
                H.a ! A.href "" $ "Graphs"
        H.div ! A.id "focuses" $ do
            H.p ! A.id "sci" ! A.class_ "focus" $ "Scientific Computing"
            H.div ! A.id "sci-details" ! A.class_ "details" $ ""
            H.p ! A.id "AI" ! A.class_ "focus" $ "Artificial Intelligence"
            H.div ! A.id "AI-details" ! A.class_ "details" $ ""
            H.p ! A.id "NLP" ! A.class_ "focus" $ "Natural Language Processing"
            H.div ! A.id "NLP-details" ! A.class_ "details" $ ""
            H.p ! A.id "vision" ! A.class_ "focus" $ "Computer Vision"
            H.div ! A.id "vision-details" ! A.class_ "details" $ ""
            H.p ! A.id "systems" ! A.class_ "focus" $ "Computer Systems"
            H.div ! A.id "systems-details" ! A.class_ "details" $ ""
            H.p ! A.id "game" ! A.class_ "focus" $ "Video Games"
            H.div ! A.id "game-details" ! A.class_ "details" $ ""
            H.p ! A.id "HCI" ! A.class_ "focus" $ "Human Computer Interaction"
            H.div ! A.id "HCI-details" ! A.class_ "details" $ ""
            H.p ! A.id "theory" ! A.class_ "focus" $ "Theory of Computation"
            H.div ! A.id "theory-details" ! A.class_ "details" $ ""
            H.p ! A.id "web" ! A.class_ "focus" $ "Web Technologies"
            H.div ! A.id "web-details" ! A.class_ "details" $ ""
        H.div ! A.id "graphs" $ ""
    H.div ! A.id "sidebar-button" $
        H.img ! A.id "sidebar-icon" ! A.src "static/res/ico/sidebar.png"
