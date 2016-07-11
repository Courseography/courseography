{-# LANGUAGE OverloadedStrings #-}

module Response.Export
    (exportResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server


exportResponse :: ServerPart Response
exportResponse =
    serveDirectory DisableBrowsing ["CSGraph.pdf"] "."