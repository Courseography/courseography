{-# LANGUAGE OverloadedStrings, CPP #-}

{-|
Description: Configure and run the server for Courseography.
This module defines the configuration for the server, including logging.
It also defines all of the allowed server routes, and the corresponding
responses.
-}
module Server
    (runServer) where

import Control.Monad (msum)
import Control.Concurrent (killThread, forkIO)
import Happstack.Server hiding (host)
import Response (notFoundResponse)
import Filesystem.Path.CurrentOS as Path
import System.Directory (getCurrentDirectory)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import System.Log.Logger (updateGlobalLogger, rootLoggerName, setLevel, Priority(INFO))
import System.Process (CreateProcess(std_in), StdStream(CreatePipe), createProcess, proc)
import Data.String (fromString)
import Config (markdownPath, serverConf)
import qualified Data.Text.Lazy.IO as LazyIO
import Routes (routes)
import GHC.IO.Handle (hClose)

webpackScript :: Path.FilePath
#ifdef mingw32_HOST_OS
webpackScript = Path.concat ["node_modules", ".bin", "webpack.cmd"]
#else
webpackScript = Path.concat ["node_modules", ".bin", "webpack"]
#endif

webpackProcess :: CreateProcess
webpackProcess = (proc (Path.encodeString webpackScript) ["--watch-stdin"])
                   {std_in = CreatePipe}

runServer :: IO ()
runServer = do
    (Just webpackIn, _, _, _) <- createProcess webpackProcess
    configureLogger
    staticDir <- getStaticDir
    aboutContents <- LazyIO.readFile $ markdownPath ++ "README.md"
    privacyContents <- LazyIO.readFile $ markdownPath ++ "PRIVACY.md"

    -- Start the HTTP server
    httpThreadId <- forkIO $ simpleHTTP serverConf $ do
      decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
      msum
           (map matchDir (routes staticDir aboutContents privacyContents) ++
           [ do
              nullDir
              seeOther ("graph" :: String) (toResponse ("Redirecting to /graph" :: String)),
              notFoundResponse
        ])
    waitForTermination
    killThread httpThreadId
    -- Closing the stdin of webpack stops the watch.
    hClose webpackIn
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
    matchDir :: (String, ServerPart Response) -> ServerPartT IO Response
    matchDir (pathname, response) = do
        noTrailingSlash -- enforce no trailing slash in the URI
        dir pathname nullDir -- enforce no segment after the specified path
        response -- perform if pathname (the URI) fully matches
