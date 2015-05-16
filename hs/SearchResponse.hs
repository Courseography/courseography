{-# LANGUAGE OverloadedStrings #-}

module SearchResponse where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts

searchResponse :: ServerPart Response
searchResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Timetable Search"
                    []
                    (do header "search"
                        H.div ! A.id "content" $ ""
                    )
                    searchScripts
