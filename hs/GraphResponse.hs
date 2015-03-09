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
                    H.div ! A.id "graph" ! A.class_ "graph" $   ""
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


