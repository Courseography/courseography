{-# LANGUAGE OverloadedStrings #-}

module SearchResponse where

import Data.List
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
                    [H.meta ! A.name "keywords"
                            ! A.content "",
                            searchLinks
                    ]
                    (do header "search"
                        H.div ! A.id "content" $ ""
                    )
                    searchScripts
