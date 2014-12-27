{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.String
import Control.Monad    (msum)
import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements
import MasterTemplate
import GridResponse
import GraphResponse
import AboutResponse

graph :: String
graph = "graph"

grid :: String
grid = "grid"

about :: String
about = "about"

static :: String
static = "static"

staticDir :: String
--staticDir = "C:\\Users\\David\\Documents\\courseography"
staticDir = "/home/cynic/4/courseography"

course :: String
course = "course"

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir grid $ gridResponse,
         dir graph $ graphResponse,
         dir about $ aboutResponse,
         dir static $ serveDirectory EnableBrowsing [] staticDir,
         dir course $ ok $ toResponse csc148
       ]

--queryC :: String -> String
--queryC course = do
--	              dx <- queryCourse course
--	              return dx

--queryCourse :: String -> IO ()
--queryCourse course = runSqlite dbStr $ do
--        let sql = "SELECT * FROM Lectures WHERE code like " ++ course
--        rawQuery sql [] $$ CL.mapM_ (liftIO . print)


csc148 :: String
csc148 = "{'prereqString': 'CSC108H1', 'F': {'lectures': [{'instructor': 'Liu', 'section': 'L0101', 'extra': 0, 'time': [[0, 9], [2, 9]], 'time_str': 'MW9', 'cap': 150}, {'instructor': 'Liu', 'section': 'L0201', 'extra': 0, 'time': [[0, 10], [2, 10]], 'time_str': 'MW10', 'cap': 150}], 'tutorials': [['T0101', [[3, 9], [3, 10]], 'R9-11'], ['T0201', [[3, 11], [3, 12]], 'R11-1'], ['T5101', [[3, 19], [3, 20]], 'R7-9']]}, 'distribution': 'This is a Science course', 'S': {'lectures': [{'instructor': 'Horton', 'section': 'L0101', 'extra': 0, 'time': [[0, 9], [2, 9]], 'time_str': 'MW9', 'cap': 240}, {'instructor': 'Heap', 'section': 'L0201', 'extra': 0, 'time': [[0, 10], [2, 10]], 'time_str': 'MW10', 'cap': 240}, {'instructor': 'Heap', 'section': 'L5101', 'extra': 0, 'time': [[2, 18], [2, 19]], 'time_str': 'W6-8', 'cap': 250}], 'tutorials': [['T0101', [[3, 9], [3, 10]], 'R9-11'], ['T0201', [[3, 11], [3, 12]], 'R11-1'], ['T0301', [[3, 13], [3, 14]], 'R1-3'], ['T0401', [[3, 15], [3, 16]], 'R3-5'], ['T5101', [[3, 17], [3, 18]], 'R5-7'], ['T5201', [[3, 19], [3, 20]], 'R7-9'], ['T0501', [[4, 11], [4, 12]], 'F11-1']]}, 'title': 'Introduction to Computer Science', 'exclusions': null, 'prereqs': null, 'description': 'Abstract data types and data structures for implementing them. Linked data structures. Encapsulation and information-hiding. Object-oriented programming. Specifications. Analyzing the efficiency of programs. Recursion. This course assumes programming experience in a language such as Python, C++, or Java, as provided byCSC108H1. Students who already have this background may consult the Computer Science Undergraduate Office for advice about skippingCSC108H1. Practical (P) sections consist of supervised work in the computing laboratory. These sections are offered when facilities are available, and attendance is required. NOTE: Students may go to their college to drop down fromCSC148H1toCSC108H1. See above for the drop down deadline.', 'name': 'CSC148H1', 'prep': null, 'breadth': 'The Physical and Mathematical Universes (5)', 'manualTutorialEnrolment': true}"