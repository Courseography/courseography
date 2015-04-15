{-# LANGUAGE OverloadedStrings #-}

module WebParsing.TimeConverter (makeTimeSlots) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist
import Database.Persist.Sqlite
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.List.Utils
import Data.Maybe
import Database.Tables as Tables
import Database.JsonParser
import WebParsing.ParsingHelp
import Control.Monad.IO.Class
import Data.Text.Read 

--converts days into numbers, returns a tuple of the rest of the string (the times)
--and a list of number representations of days
convertTime :: (T.Text, [Double]) -> (T.Text, [Double])
convertTime (str, times)
   |T.null str = ("", times)
   |T.head str == 'M' = convertTime (T.drop 1 str, 0:times)
   |T.head str == 'T' = convertTime (T.drop 1 str, 1:times)
   |T.head str == 'W' = convertTime (T.drop 1 str, 2:times)
   |T.head str == 'R' = convertTime (T.drop 1 str, 3:times)
   |T.head str == 'F' = convertTime (T.drop 1 str, 4:times)
   |otherwise = (str, times)

--given to numbers, creates a list of half-hours intervals between them
--modifies 12-hour representation to 24
makeSlots :: Double -> Double -> [Double]
makeSlots start end =
  if end < start
  then halfHourSlots start (12+end)
  else  if start < 8.5
        then halfHourSlots (start + 12) (end + 12)
        else (halfHourSlots start end)

--extends functionality of makeSlots
halfHourSlots :: Double -> Double -> [Double]
halfHourSlots start end = 
  let hours = [start .. end-1]
  in concat $ map (\h -> [h,  h+0.5]) hours

--converts the textual representation of time into numbers
toDouble :: T.Text -> Double
toDouble double = 
  let splitIt = T.split (== ':') double
      list = reads $ T.unpack (head splitIt)
      halfHour= if (length splitIt) > 1
                then 0.5
                else 0
  in  if (length list) == 0
      then 0.0
      else (fst (head list)) + halfHour

--zips togethor time slots with their days
addList :: [a] -> [a] -> [[a]]
addList days slots = 
  concat $ map (\day -> map (\time -> [day,time]) slots) days

--Takes in a string representation of timeslots, returns a list of days and half-hour timeslots
makeTimeSlots  :: T.Text -> [[Double]]
makeTimeSlots str = 
  let (times, days) = convertTime (str, [])
      doubles = map toDouble ( T.split (== '-') times)
      slots = if (length doubles) == 1
              then makeSlots (head doubles) ((head doubles) + 1)
              else makeSlots (head doubles) (head (tail doubles))
      in addList days slots

