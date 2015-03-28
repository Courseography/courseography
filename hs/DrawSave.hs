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
-- map the insert functions to the arrays of JSON objects
-- insert functions for single graph
--                  - for 

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

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Text.Internal as TI
import Database.JsonParser

-- JsonParser.hs
-- | Inserts the tutorials from a specified section into the Tutorials table.
insertSessionTutorials :: MonadIO m => Maybe Session -> T.Text -> Course -> ReaderT SqlBackend m ()
insertSessionTutorials Nothing sessionStr course = return ()
insertSessionTutorials (Just session) sessionStr course = 
    (unless $ null (tutorials session)) $ 
    mapM_ (insertTutorial sessionStr course) (tutorials session)


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