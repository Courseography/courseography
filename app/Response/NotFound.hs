module Response.NotFound
    (notFoundResponse) where

import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Blaze

notFoundResponse :: ServerPart Response
notFoundResponse =
  notFound $ toResponse $
    H.html $ do
        H.head $ do
            H.title "Courseography - 404!"
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            toStylesheet "/static/style/app.css"

        H.body notFoundContent

notFoundContent :: H.Html
notFoundContent =
    H.div ! A.id "contentDiv" $ do
        H.h2 "404 Page Not Found!"
        H.p "Sorry, the path you have traversed has no destination node."
        H.p "The page might have been moved or deleted, or the little dragon running our server might have gone to have smores."
        H.p "You can use the links below to get back on the grid or graph."
        H.ul ! A.id "links" $ do
            H.li $ toLink "/graph" "Graph"
            H.li $ toLink "/grid" "Grid"
