{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.String
import Control.Monad    (msum)
--import template
import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
                 H.link ! A.rel "stylesheet"
                        ! A.type_ "text/css"
                        ! A.href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                 H.link ! A.rel "stylesheet"
                        ! A.type_ "text/css"
                        ! A.href "static/style/grid/timetable_styles.css"
                ]
                (do  H.div  ! A.class_ "row header" $ do
                         H.div ! A.class_ "col-md-6 col-xs-6"
                               ! A.id "header" $ do
                                H.h2 $
                                    "2014-2015 Timetable"
                         H.div ! A.class_ "col-md-6 col-xs-6" $
                                H.a
                               ! A.href "static/planner.html" $ do
                                H.h2 ! A.id "home-link" $
                                    "Back to Graph"

                     H.div ! A.id "dialog" $ do
                         "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

                     H.div ! A.class_ "row main" $ do
                         H.div ! A.class_ "col-md-2 col-xs-6"
                               ! A.id "course-select-wrapper" $ do
                               H.ul ! A.id "course-select"
                                    ! A.class_ "trapScroll-enabled" $ do
                                        H.li ! A.id "clear-all" $ do
                                            H.h3 "Clear All"

                         H.div ! A.class_ "col-md-2 col-xs-6 col-md-push-8"
                               ! A.id "search-layout" $ do
                                 H.div ! A.id "filter-container" $ do
                                     H.form ! A.onsubmit "return false;" $ do
                                         H.input ! A.type_ "text"
                                                 ! A.id "course-filter"
                                                 ! A.class_ "form-control"
                                                 ! A.placeholder "Enter a course!"
                                                 ! A.autocomplete "off"
                                 H.div ! A.id "search-container" $ do
                                     H.div ! A.id"search-list" $ ""

                         H.div ! A.class_ "col-md-8 col-xs-12 col-md-pull-2" $ do
                             H.div ! A.id "info"
                                   ! A.class_ "row" $ do
                                 H.div ! A.id "info-layout" $ do
                                     H.h2 $ do
                                         H.span ! A.id "course-info-code" $ ""
                                         H.span ! A.id "course-info-title"  $ ""

                                     H.h4 $ do
                                         H.span ! A.id "section-stats-section" $ ""
                                         H.span ! A.id "section-stats-instructor" $ ""

                                     H.p ! A.id "section-stats-enrol" $ ""
                         )

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