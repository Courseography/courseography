module Controllers.Generate
    (generateResponse, findAndSavePrereqsResponse) where

import Control.Monad ()
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, object, (.=))
import Data.List (nub)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import qualified Data.Text.Lazy as TL
import DynamicGraphs.GraphOptions (CourseGraphOptions (..), GraphOptions (..))
import DynamicGraphs.WriteRunDot (generateAndSavePrereqResponse, getBody)
import Happstack.Server
import MasterTemplate
import Models.Post (reqsForPost, returnPost)
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Happstack (createJSONResponse)

generateResponse :: ServerPart Response
generateResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Generate"
                    []
                    (do
                        H.div ! A.id "navbar" $ ""
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

    postResults <- liftIO $ mapM (\code -> do
                        post <- returnPost (TL.toStrict code)
                        return (TL.toStrict code, post))
                   (programs coursesOptions)

    let invalidPrograms = map fst $ filter (isNothing . snd) postResults
        validPrograms = mapMaybe snd postResults

    allCourses <- liftIO $ nub <$>
        if all (== TL.empty) (courses coursesOptions)
            then return $ map TL.pack (concatMap reqsForPost validPrograms)
            else return $ courses coursesOptions

    let updatedCoursesOptions = coursesOptions
            { courses = map TL.toUpper allCourses
            , graphOptions = (graphOptions coursesOptions)
                { taken = map TL.toUpper (taken (graphOptions coursesOptions))
                , departments = map TL.toUpper (departments (graphOptions coursesOptions))
                }
            }

    if all (== TL.empty) (courses coursesOptions) && not (null invalidPrograms)
        then return $ createJSONResponse $ object ["error" .= object ["invalidPrograms" .= invalidPrograms]]
        else liftIO $ generateAndSavePrereqResponse updatedCoursesOptions
