module Controllers.Generate
    (generateResponse, findAndSavePrereqsResponse) where

import Happstack.Server
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, object, (.=))
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import Database.CourseQueries (returnPost, reqsForPost)
import DynamicGraphs.WriteRunDot (getBody, generateAndSavePrereqResponse)
import DynamicGraphs.GraphOptions (CourseGraphOptions (..), GraphOptions (..))
import Util.Happstack (createJSONResponse)

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
    
    invalidPrograms <- liftIO $
        filterM (fmap (== Nothing) . returnPost) (map TL.toStrict (programs coursesOptions))

    allCourses <- liftIO $
        if all (null . TL.unpack) (courses coursesOptions)
            then nub . concat <$> mapM (reqsForPost . TL.toStrict) (programs coursesOptions)
            else return . nub . map TL.unpack $ courses coursesOptions

    let updatedCoursesOptions = coursesOptions 
            { courses = map (TL.toUpper . TL.pack) allCourses
            , graphOptions = (graphOptions coursesOptions) 
                { taken = map TL.toUpper (taken (graphOptions coursesOptions))
                , departments = map TL.toUpper (departments (graphOptions coursesOptions))
                }
            }

    if all (null . TL.unpack) (courses coursesOptions) && not (null invalidPrograms)
        then return $ createJSONResponse $ object ["texts" .= map (object . (:[]) . ("text" .=)) invalidPrograms]
        else liftIO $ generateAndSavePrereqResponse updatedCoursesOptions
