{-# LANGUAGE OverloadedStrings #-}

module FourOhFourResponse
    (fourOhFourResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import Utilities
import Scripts

fourOhFourResponse :: ServerPart Response
fourOhFourResponse =
  notFound $ toResponse $
    H.html $ do
        H.head $ do
            H.title "Courseography - 404!"
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_  $ map stylesheet [
                "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                "static/style/app.css"]

        H.body $ do
            fourOhFourContent
            fourOhFourScripts

fourOhFourContent :: H.Html
fourOhFourContent =
    H.div ! A.id "contentDiv" $ do
        H.h2 "404 Page Not Found!"
        H.p "Sorry, the path you have traversed has no destination node."
        H.p "The page might have been moved or deleted, or the little dragon running our server might have gone to have smores."
        H.p "You can use the links below to get back on the grid or graph."
        H.ul ! A.id "links" $ do
            H.li $ makeA "" "" "graph" "" $ "Graph"
            H.li $ makeA "" "" "grid" "" $ "Grid"
