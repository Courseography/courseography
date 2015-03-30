{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module DrawResponse where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts
import Data.Aeson
import Data.Maybe
import Data.List as L
import Database.Persist.Sqlite
import Database.Persist
import Database.Tables
import System.Directory
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)

import Data.Data (Data, Typeable)

drawResponse :: ServerPart Response
drawResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Draw!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 drawLinks
                ]
                (do
                    header "draw"
                    drawHtml
                    modePanel
                )
                drawScripts


drawHtml :: H.Html
drawHtml = H.div ! A.id "aboutDiv" $ do
  H.h2 "Draw a Graph"

modePanel :: H.Html
modePanel = createTag H.div "side-panel-wrap" "" $ do
  createTag H.div "mode-panel" "" $ do 
  createTag H.div "node-mode" "mode clicked" "NODE (n)"
  H.input ! A.id "course-code" ! A.class_ "course-code" ! A.name "course-code" ! A.placeholder "Course Code" ! A.autocomplete "off" ! A.type_ "text" ! A.size "10"
  createTag H.div "add-text" "button" "ADD"
  createTag H.div "red" "colour clicked" "RED"
  createTag H.div "green" "colour" "GREEN"
  createTag H.div "blue" "colour" "BLUE"
  createTag H.div "purple" "colour" "PURPLE"
  createTag H.div "path-mode" "mode" "PATH (p)" 
  createTag H.div "region-mode" "mode" "REGION (r)"
  createTag H.div "finish-region" "button" "finish (f)" 
  createTag H.div "change-mode" "mode" "SELECT/MOVE (m)" 
  createTag H.div "erase-mode" "mode" "ERASE (e)"
  createTag H.div "save" "button" "SAVE (s)"

{-
-- code from: http://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
-- put this function in a library somewhere
getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return ""

save :: String -> ServerPart Response
save objs = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  case d of
  Left err -> putStrLn err
  Right ps -> print ps

instance FromJSON Person where
 parseJSON (Object v) =
    Point <$> v .: "x"
           <*> v .: "y"
 parseJSON _ = mzero


data Point = Point { x :: Int, y :: Int } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

-- | Returns an image of the timetable requested by the user.
save :: ServerPart Response
save = do
    body <- getBody -- it's a ByteString
    let unit = fromJust $ decode body :: Unit -- how to parse json
    ok $ toResponse $ encode unit -- how to send json back.

    -- liftIO $ getTimetableImage courses 
-}