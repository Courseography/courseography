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