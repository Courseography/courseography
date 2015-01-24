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
    masterTemplate "Courseography - SVG serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do
                    header "about"
                    aboutHtml aboutContents
                )
                ""

aboutHtml :: String -> H.Html
aboutHtml contents = H.div ! A.id "aboutDiv" $ writeHtml def $ readMarkdown def $ contents