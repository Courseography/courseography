module Response.About
    (aboutResponse) where

import Data.Text.Lazy (Text)
import Happstack.Server
import MasterTemplate
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Blaze (mdToHTML)
import Scripts (aboutScripts)

aboutResponse :: Text -> ServerPart Response
aboutResponse aboutContents =
   ok $ toResponse $
    masterTemplate "Courseography - About"
                []
                (do
                    header "about"
                    aboutHtml aboutContents
                )
                aboutScripts

-- | AboutHtml takes in the contents of the README.md file (the GitHub README file) and translates
-- the markdown to blaze-HTML.
aboutHtml :: Text -> H.Html
aboutHtml contents = H.div ! A.id "aboutDiv" $ mdToHTML contents
