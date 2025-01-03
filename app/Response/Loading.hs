module Response.Loading
    (loadingResponse) where

import Happstack.Server
import MasterTemplate
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

loadingResponse :: ServerPart Response
loadingResponse = do
   size <- lookText' "size"
   ok $ toResponse $
    masterTemplate "Courseography - Loading..."
                []
                (do
                    header "Loading..."
                    if size == "small"
                        then smallLoadingIcon
                        else largeLoadingIcon
                )
                ""

{- Insert a large loading icon into the page -}
largeLoadingIcon :: H.Html
largeLoadingIcon = H.div ! A.id "loading-icon" $ do
              H.img ! A.id "c-logo" ! A.src "/static/res/img/C-logo.png"
              H.img ! A.id "compass" ! A.class_ "spinner" ! A.src "/static/res/img/compass.png"

{- Insert a small loading icon into the page -}
smallLoadingIcon :: H.Html
smallLoadingIcon = H.div ! A.id "loading-icon" $ do
              H.img ! A.id "c-logo-small" ! A.src "/static/res/img/C-logo-small.png"
              H.img ! A.id "compass-small" ! A.class_ "spinner" ! A.src "/static/res/img/compass-small.png"
