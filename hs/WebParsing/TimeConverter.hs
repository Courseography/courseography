{-# LANGUAGE OverloadedStrings #-}

module WebParsing.TimeConverter
    (makeTimeSlots) where

import qualified Data.Text as T
import Database.Tables

-- | Converts days into numbers, returns a tuple of the rest of the string (the times)
-- and a list of number representations of days
convertTime :: (T.Text, [Double]) -> (T.Text, [Double])
convertTime (str, times)
   |T.null str = ("", times)
   |T.head str == 'M' = convertTime (T.drop 1 str, 0:times)
   |T.head str == 'T' = convertTime (T.drop 1 str, 1:times)
   |T.head str == 'W' = convertTime (T.drop 1 str, 2:times)
   |T.head str == 'R' = convertTime (T.drop 1 str, 3:times)
   |T.head str == 'F' = convertTime (T.drop 1 str, 4:times)
   |otherwise = (str, times)

-- | Given to numbers, creates a list of half-hours intervals between them
-- modifies 12-hour representation to 24
makeSlots :: Double -> Double -> [Double]
makeSlots start end
    | end < start = halfHourSlots start (12 + end)
    | start < 8.5 = halfHourSlots (start + 12) (end + 12)
    | otherwise   = halfHourSlots start end

-- | Extends functionality of makeSlots
halfHourSlots :: Double -> Double -> [Double]
halfHourSlots start end =
    let hours = [start .. end - 1]
    in concatMap (\h -> [h,  h + 0.5]) hours

-- | Converts the textual representation of time into numbers
toDouble :: T.Text -> Double
toDouble double
    | null list = 0.0
    | otherwise = fst (head list) + halfHour
    where
        splitIt = T.split (== ':') double
        list = reads $ T.unpack (head splitIt)
        halfHour = if length splitIt > 1
                   then 0.5
                   else 0

-- | Zips together time slots with their days
addList :: [a] -> [a] -> [[a]]
addList days slots =
    concatMap (\day -> map (\time -> [day,time]) slots) days

-- | Returns a list of days and half-hour timeslots.
makeTimeSlots  :: T.Text -> [Time]
makeTimeSlots str =
    let (times, days) = convertTime (str, [])
        doubles = map toDouble ( T.split (== '-') times)
        slots = if length doubles == 1
                then makeSlots (head doubles) (head doubles + 1)
                else makeSlots (head doubles) (head (tail doubles))
    in map Time (addList days slots)
