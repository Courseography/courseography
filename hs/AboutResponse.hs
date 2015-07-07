{-# LANGUAGE OverloadedStrings #-}

module AboutResponse
    (aboutResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (Text)
import Happstack.Server
import MasterTemplate
import Utilities (mdToHTML)

aboutResponse :: Text -> ServerPart Response
aboutResponse aboutContents =
   ok $ toResponse $
    masterTemplate "Courseography - About"
                []
                (do
                    header "about"
                    aboutHtml aboutContents
                )
                ""

-- | AboutHtml takes in the contents of the README.md file (the GitHub README file) and translates
-- the markdown to blaze-HTML.
aboutHtml :: Text -> H.Html
aboutHtml contents = H.div ! A.id "aboutDiv" $ mdToHTML contents
