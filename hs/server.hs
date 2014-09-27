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
                (H.p $ do "Hello, "
                          H.b "David Liu!!"
                          H.b " and Ian!!"
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