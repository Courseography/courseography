{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.String
import Control.Monad    (msum)
import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements

gridTemplate :: String -> [H.Html] -> H.Html -> H.Html
gridTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body


graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    gridTemplate "Courseography - Grid"
                [H.meta ! A.name "keywords"
                        ! A.content "happstack, david, liu",
                 makeLink "stylesheet" "text/css" "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                 makeLink "stylesheet" "text/css" "static/style/grid/timetable_styles.css"
                ]
                (do  makeDiv "" "row header" $ do
                         makeDiv "header" "col-md-6 col-xs-6" $ do
                                makeH2 "" "" "2014-2015 Timetable"
                         makeDiv "" "col-md-6 col-xs-6" $ do
                                makeA "" "" "static/planner.html" $ do
                                    makeH2 "home-link" "" "Back to Graph"

                     makeDiv "dialog" "" "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

                     makeDiv "" "row main" $ do
                         makeDiv "course-select-wrapper" "col-md-2 col-xs-6" $ do
                               makeUl "course-select" "trapScroll-enabled" $ do
                                        makeLi "clear-all" "" $ do
                                            makeH3 "" "" "Clear All"

                         makeDiv "search-layout" "col-md-2 col-xs-6 col-md-push-8" $ do
                                 makeDiv "filter-container" "" $ do
                                     makeForm "" "" "return false;" $ do
                                         makeInput "course-filter" "form-control" "Enter a course!" "off" "text"
                                 makeDiv "search-container" "" $ do
                                     makeDiv "search-list" "" ""

                         makeDiv "" "col-md-8 col-xs-12 col-md-pull-2" $ do
                             makeDiv "info" "row" $ do
                                 makeDiv "info-layout" "" $ do
                                     makeH2 "" "" $ do
                                         makeSpan "course-info-code" "" ""
                                         makeSpan "course-info-title" "" ""

                                     makeH4 "" "" $ do
                                         makeSpan "section-stats-section" "" ""
                                         makeSpan "section-stats-instructor" "" ""

                                     makeP "section-stats-enrol" "" ""

                     -- Include all compiled plugins (below), or include individual files as needed -->

                     insertTimetableScripts
                         )



insertTimetableScripts :: H.Html
insertTimetableScripts = do insertjQuery
                            makeScript "static/js/grid/timetable_util.js"
                            makeScript "static/js/grid/setup.js"
                            makeScript "static/js/grid/mouse_events.js"
                            makeScript "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
                            makeScript "http://code.jquery.com/ui/1.10.4/jquery-ui.js"
                            makeScript "static/js/common/cookieHandler.js"
                            makeScript "static/js/grid/generate_grid.js"
                            makeScript "static/js/common/objects/course.js"
                            makeScript "static/js/common/objects/section.js"
                            makeScript "static/js/common/utilities/util.js"

insertjQuery :: H.Html
insertjQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
graph :: String
graph = "planner.html"

grid :: String
grid = "timetable_creator.html"

static :: String
static = "static"

staticDir :: String
staticDir = "/home/cynic/2/cscourseplanner"

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir graph $ graphResponse,
         dir grid $ graphResponse,
         dir static $ serveDirectory DisableBrowsing [] staticDir
       ]