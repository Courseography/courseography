module Controllers.Generate
    (generateResponse, findAndSavePrereqsResponse) where

import Happstack.Server
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import DynamicGraphs.WriteRunDot (getBody, generateAndSavePrereqResponse)
import DynamicGraphs.GraphOptions (CourseGraphOptions (..))
import Data.Maybe (fromJust)
import Data.Aeson (decode)
import Control.Monad.IO.Class (liftIO)

generateResponse :: ServerPart Response
generateResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Generate"
                    []
                    (do
                        header "generate-prerequisites"
                        generatePrerequisites
                    )
                    generateScripts

generatePrerequisites :: H.Html
generatePrerequisites =
    H.html $ do
        H.head $
            H.title "Generate Prerequisites!"
        H.div ! A.id "generateRoot" $ ""

findAndSavePrereqsResponse :: ServerPart Response
findAndSavePrereqsResponse = do
    method PUT
    requestBody <- getBody
    let coursesOptions :: CourseGraphOptions = fromJust $ decode requestBody
    liftIO $ generateAndSavePrereqResponse coursesOptions
