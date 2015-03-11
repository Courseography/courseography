{-# LANGUAGE OverloadedStrings #-}

module FourOhFourResponse where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts

fourOhFourResponse :: ServerPart Response
fourOhFourResponse =
   ok $ toResponse $
    H.html $ do
        H.head $ do
            H.title (H.toHtml "Courseography - 404!")
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_  [fourOhFourLinks]
        H.body $ do
            fourOhFourHtml
            fourOhFourScripts

fourOhFourHtml :: H.Html
fourOhFourHtml = H.div ! A.id "aboutDiv" $ do
  H.h2 "404 Page Not Found!"
  H.p "Sorry, the path you have traversed has no destination node."
  H.p "The page might have been moved or deleted, or the little dragon running our server might have gone to have smores."
  H.p "You can use the links above to get back on the grid(, graph etc.)."
  createTag H.div "picDiv" "" "" -- dragon

