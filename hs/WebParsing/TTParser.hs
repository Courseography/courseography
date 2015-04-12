

convertTime :: (String -> [[Int]]) -> (T.Text, [[Int]])
convertTime (str, times) = 
   |T.head str == 'M' = (tail str, [0]:times)
   |T.head str == 'T' = (tail str, [1]:times)
   |T.head str == 'W' = (tail str, [2]:times)
   |T.head str == 'R' = (tail str, [3]:times)
   |T.head str == 'F' = (tail str, [4]:times)
   |else = (str, times)

addSlot :: T.Text -> [[Int]] -> [[Int]]
addSlot str times =
    let times = T.split "-" str
        toInt = map decimal times
        extractInts = map extractInt toInt
    in makeSlots extractInts 

makeSlots :: Int -> Int -> [Int]
makeSlots start end =
  if start <= 12 && end > 12
  then 
  else 

halfHourSlots :: Int -> Int -> [Float]
halfHourSlots start end = 
  let hours = [start .. end-1]
  in concat $ map [\h -> [h,  h+0.5]] hours
