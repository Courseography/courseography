{-|
Description: Configure and run the server for Courseography.
This module defines the configuration for the server, including logging.
It also defines all of the allowed server routes, and the corresponding
responses.
-}
module Server
    (runServer) where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server hiding (host)
import Response
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList, getGraphJSON)
import Database.CourseInsertion (saveGraphJSON)
import Filesystem.Path.CurrentOS as Path
import System.Directory (getCurrentDirectory)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import System.Log.Logger (updateGlobalLogger, rootLoggerName, setLevel, Priority(INFO))
import Data.String (fromString)
import FacebookUtilities
import Config (markdownPath, serverConf)
import qualified Data.Text.Lazy.IO as LazyIO
import Data.Int (Int64)
import Route (routes)

runServer :: IO ()
runServer = do
    configureLogger
    staticDir <- getStaticDir
    redirectUrlGraphEmail <- retrieveAuthURL testUrl
    redirectUrlGraphPost <- retrieveAuthURL testPostUrl
    aboutContents <- LazyIO.readFile $ markdownPath ++ "README.md"
    privacyContents <- LazyIO.readFile $ markdownPath ++ "PRIVACY.md"

    -- Start the HTTP server
    simpleHTTP serverConf $ do
      decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
      msum  
           (map (\ (a, b) -> dir a b) $ routes) ++
           [ do
               nullDir
               seeOther "graph" (toResponse "Redirecting to /graph"),    
               notFoundResponse
        ]

    where
    -- | Global logger configuration.
    configureLogger :: IO ()
    configureLogger = do
        -- Use line buffering to ensure logging messages are printed correctly
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
        -- Set log level to INFO so requests are logged to stdout
        updateGlobalLogger rootLoggerName $ setLevel INFO

    -- | Return the directory where all static files are stored.
    -- Note: the type here is System.IO.FilePath, not FileSystem.Path.FilePath.
    getStaticDir :: IO Prelude.FilePath
    getStaticDir = do
        cwd <- getCurrentDirectory
        --let parentDir = Path.parent $ Path.decodeString cwd
        --return $ Path.encodeString $ Path.append parentDir $ fromString "public/"
        return $ Path.encodeString $ Path.append (Path.decodeString cwd) $ fromString "public/"
