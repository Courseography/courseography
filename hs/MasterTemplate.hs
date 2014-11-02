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


-- Insert the header of the Grid and Graph. This contains the year of the timetable, and
-- a link back to the Graph.
header :: String -> H.Html
header page = createTag H.nav "" "row header" $ do
  H.h2 "Courseography"
  H.ul ! A.id "nav-links" $ do
    H.li $ makeA "" "" "graph" "" $ "Graph"
    H.li $ makeA "" "" "grid" "" $ "Grid"
    H.li $ makeA "" "" "" "" $ "Check My POSt!"
    H.li $ makeA "" "" "" "" $ "About"
