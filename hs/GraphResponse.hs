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
    masterTemplate "Courseography - SVG serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 plannerLinks
                ]
                (do header "graph"
                    createTag H.div "graph" "graph" ""
                    createTag H.div "" "infoTabs" $ do
                        tabList $ do
                            welcomeTab
                            timetableTab
                )
                plannerScripts


tabList :: H.Html -> H.Html
tabList content = createTag H.div "" "tabListDiv" $ do
                  createTag H.ul "" "tabList" $ do
                      H.li $ tabAnchor "#welcome" "Welcome!"
                      H.li $ tabAnchor "#focuses" "Focuses"
                      H.li $ tabAnchor "#timetable" "Timetable"
                      H.li $ tabAnchor "#post" "Check My POSt!"
                      do content
                  fceCountDiv


fceCountDiv :: H.Html
fceCountDiv = createTag H.div "FCECountDiv" "" $ do
                  createTag H.span "FCEcount" "" $ do "0.0"
                  "FCEs" -- Being difficult. Won't show up correctly.

welcomeTab :: H.Html
welcomeTab = createTag H.div "welcome" "infoTab" $ do
                     createTag H.div "" "infoTabContent" $ do
                         H.h2 $ do "Welcome!"
                         H.p $ do
                             "The graph above displays the prerequisite links connecting courses "
                             "in our department. Select courses to plan your enrolments for "
                             "future terms! Courses that you've selected but have missing "
                             "prerequisites will be highlighted in red. "

                         H.p $ do
                             "Check out the different tabs to access helpful features for your "
                             "planning. Also, here's a"
                             makeA "" "" "res/full_graph.jpg" "_blank" $ do "printable version "
                             "of the graph."

                         --createTag H.br "" ""
                         H.p $ do
                             "Courseography is an ongoing "
                             makeA "" "" "https://github.com/Ian-Stewart-Binks/courseography" "_blank" $ do "project "
                             "maintained by Ian Stewart-Binks and"
                             makeA "" "" "http://www.cs.toronto.edu/~liudavid/" "_blank" $ do " David Liu "
                             "."
                             "Ideas for new features, better design, and (especially) bug reports"
                             "are always welcome!"
                             "Please send all feedback to"
                             makeA "" "" "mailto:cs.toronto.courseplanner@gmail.com" "_blank" $ do " this address "
                             "."
                             "If you see a bug, please do let us know which browser and version you're using."
                             "And if there's a display issue, giving us your screen display info"
                             "(e.g., resolution) will be rather helpful. Thanks!"

focusesTab :: H.Html
focusesTab = ""

timetableTab :: H.Html
timetableTab = createTag H.div "timetable" "infoTab" $ do
                           createTag H.div "timetableSearch" "infoTabContent" $ do
                               createTag H.h2 "" ""  $ do
                                   "2014-2015 Timetable"
                               H.p "Search through the timetable for a course or instructor."
                               H.p "The \"(+5)\" caps are extra reserved seats. See official timetable for details."
                               createTag H.p "timetable-creator-link" "" $ do
                                   makeA "" "" "timetable_creator.html" "" $ do
                                       "Plan your timetable here!"
                               makeInput "filter" "text-input" "" "" "text"
                           createTag H.div "timetableContainer" "" $ do
                               ""

checkMyPostTab :: H.Html
checkMyPostTab = ""

