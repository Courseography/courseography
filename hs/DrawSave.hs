{- SCHEMA

Graph
    gId Int64
    title String
    deriving Show

Text
    gId Int64
    rId String
    pos Point
    text String
    deriving Show

Shape
    gId Int64
    id_ String
    pos Point
    width Double
    height Double
    fill String
    stroke String
    text [Text]
    tolerance Double
    type_ ShapeType

Path
    gId Int64
    id_ String
    points [Point]
    fill String
    stroke String
    isRegion Bool
    source String
    target String
    deriving Show
-}


{- STEPS
-- ajax request?
-- convert svg elements that need to be saved into JSON
-- read JSON objects into haskell, 
-- insert into Graph values gId?, "draw"

-}

{- JS CODE 
-- necessary?
new Ajax.Request("url", {method: "post",
                          onSuccess: functionName}
);


var json_text = JSON.stringify(your_object, null, 2);

function functionName(ajax) {
    do something with ajax.responseXML;
}
-}

-- HASKELL CODE
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.List as L
import Database.Persist.Sqlite
import Database.Persist
import Database.Tables
import System.Directory
import GHC.Generics

-- need res?
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Text.Internal as TI
import Database.JsonParser

-- for debugging?
-- SvgDatabase.hs
queryDatabase sql = runSqlite dbStr $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)




-- parser.hs
insertElements :: ([Path], [Shape], [Text]) -> IO ()
insertElements (paths, shapes, texts) =
    runSqlite dbStr $ do
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts


-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Person
instance ToJSON Person
