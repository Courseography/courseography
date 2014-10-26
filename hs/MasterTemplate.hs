{-# LANGUAGE OverloadedStrings #-}

module MasterTemplate where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements

masterTemplate :: String -> [H.Html] -> H.Html -> H.Html -> H.Html
masterTemplate title headers body scripts =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body
        scripts


-- Insert the header of the Grid. This contains the year of the timetable, and
-- a link back to the Graph.
header :: String -> H.Html
header page = createTag H.div "" "row header" $ do
                            createTag H.div "header" "col-md-6 col-xs-6" $ do
                                            createTag H.h2 "" "" "Courseography"
                            createTag H.div "" "col-md-6 col-xs-6" $ do
                                          if page == "graph" then
                                              makeA "" "" "grid" "" $ do
                                              createTag H.h2 "home-link" "" "Back to Grid"
                                          else
                                              makeA "" "" "graph" "" $ do
                                              createTag H.h2 "home-link" "" "Back to Graph"

