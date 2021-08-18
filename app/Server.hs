{-# LANGUAGE CPP, OverloadedStrings #-}

{-|
Description: Configure and run the server for Courseography.
This module defines the configuration for the server, including logging.
It also defines all of the allowed server routes, and the corresponding
responses.
-}
module Server
    (runServer) where

import Config (markdownPath, serverConf)
import Control.Concurrent (forkIO, killThread)
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as LazyIO
import Filesystem.Path.CurrentOS as Path
import Happstack.Server hiding (host)
import Routes (routeResponses)
import System.Directory (getCurrentDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Log.Logger (Priority (INFO), rootLoggerName, setLevel, updateGlobalLogger)
import System.Process (CreateProcess, createProcess, shell)

webpackProcess :: CreateProcess
webpackProcess = shell "yarn run watch"

runServer :: IO ()
runServer = do
    _ <- createProcess webpackProcess
    configureLogger
    staticDir <- getStaticDir
    aboutContents <- LazyIO.readFile $ markdownPath ++ "README.md"
    privacyContents <- LazyIO.readFile $ markdownPath ++ "PRIVACY.md"

    -- Start the HTTP server
    httpThreadId <- forkIO $ simpleHTTP serverConf $ do
      decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
      routeResponses staticDir aboutContents privacyContents
    waitForTermination
    killThread httpThreadId
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
