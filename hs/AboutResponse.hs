{-# LANGUAGE OverloadedStrings #-}

module AboutResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import Text.Pandoc
import MasterTemplate

aboutResponse :: String -> ServerPart Response
aboutResponse aboutContents =
   ok $ toResponse $
    masterTemplate "Courseography - About"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do
                    header "about"
                    aboutHtml aboutContents
                )
                ""

-- | AboutHtml takes in the contents of the README.md file (the GitHub README file) and translates
-- the markdown to blaze-HTML.
aboutHtml :: String -> H.Html
aboutHtml contents = H.div ! A.id "aboutDiv" $ mdToHTML contents

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to 
-- blaze-HTML.
mdToHTML :: String -> H.Html
mdToHTML contents = writeHtml def $ readMarkdown def contents