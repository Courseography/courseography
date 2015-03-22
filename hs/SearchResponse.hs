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
                        H.div ! A.id "timetableSearch" $ do
                            H.h2 "2014-2015 Timetable"
                            H.p "Search through the timetable for a course or instructor."
                            H.input ! A.type_ "text" ! A.class_ "text-input" ! A.id "filter" ! A.placeholder "Search..."
                        H.div ! A.id "timetableContainer" $ ""
                    )
                    searchScripts
