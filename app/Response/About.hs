module Response.About
    (aboutResponse) where

import Happstack.Server
import MasterTemplate
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Scripts (aboutScripts)

aboutResponse :: ServerPart Response
aboutResponse =
   ok $ toResponse $
    masterTemplate "Courseography - About"
                []
                (do
                    header "about"
                    H.div ! A.id "aboutDiv" $ ""
                )
                aboutScripts
