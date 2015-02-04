module Main where

import Control.Monad    (msum)
import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import GridResponse
import GraphResponse
--import AboutResponse
import CourseQueries
import CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import qualified Data.Text as T

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    contents <- readFile "../README.md"
    simpleHTTP nullConf $
        msum [ dir "grid" gridResponse,
               dir "graph" graphResponse,
               --dir "about" $ aboutResponse contents,
               dir "static" $ serveDirectory EnableBrowsing [] staticDir,
               dir "course" $ path (\s -> liftIO $ queryCourse (T.pack s))
             ]
