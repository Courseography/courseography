--| UberGoal: go from T.Text -> [Entity Lecture] / [Entity Tutorial] without multiple DB queries.

import Data.Tuple.Select as Tup -- selecting first, second and third elements of tuples (fall, spring, year)

-- | Step 1: single query for lecture or tutorial. Produces list of all 
-- | lectures/tutorials respectively, whcih are not separated by session

-- | Queries db for all matching records with lecture or tutorial code of this course
lectureQuery :: T.Text <- [Entity Lecture]
lectureQuery courseCode = selectList [LectureCode ==. courseCode] []

tutorialQuery :: T.Text <- [Entity Tutorial]
tutorialQuery courseCode = selectList [TutorialCode ==. courseCode] []


-- | Step 2: find a way to split these into separate lists.

-- | goal: refactor both below to something like
-- | splitSessions :: Entity => [a] -> ([a], [a], [a]) 

-- | Returns a 3-element tuple representing Fall, Spring, and Yearlong entity lists.
-- | problem: Data.Text vs String
splitSessionsL :: [Entity Lecture] -> ([Entity Lecture], [Entity Lecture], [Entity Lecture])
splitSessionsL lecturesList = do
    fallLec <- getBySessionL lecturesList "F"
    springLec <- getBySessionL lecturesList "S"
    yearLec <- getBySessionL lecturesList "Y"

    return (fallLec, springLec, yearLec)

splitSessionsT :: [Entity Tutorial] -> ([Entity Tutorial], [Entity Tutorial], [Entity Tutorial])
splitSessionsT tutList = do
    fallTut <- getBySessionT tutList "F"
    springTut <- getBySessionT tutList "S"
    yearSession <- getBySessionT tutList "Y"

    return (fallTut, springTut, yearTut)


-- | Step 2.5: Helper function to separate lists.
-- | However, this is inefficient because it requires 3 separate calls to the [Entity] list.
-- | There is probably a way to go directly to a tuple of lists.

-- | goal: refactor both below to something like
-- | getBySession :: [EntityField a] -> [Entity a] -> [Entity a]

-- | Returns the matching lecture entities if sessions correspond with session code.
  -- bb: need Data.Text not string!
getBySessionL :: String -> [Entity Lecture] -> [Entity Lecture]
getBySessionL xs sessionStr = [ x | x <- xs, (LectureSession x) == sessionStr ] 

getBySessionT :: String -> [Entity Tutorial] -> [Entity Tutorial]
getBySessionT xs sessionStr = [ x | x <- xs, (TutorialSession x) == sessionStr ]


-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. courseStr] []
    -- Query once each for lec and tut, then build session of each 
    allLectures <- splitSessionsL $ lectureQuery courseStr
    allTutorials <- splitSessionsT $ tutorialQuery courseStr    
    let fallSession   = buildSession Tup.sel1(allLectures), Tup.sel1(allTutorials) 
        springSession = buildSession Tup.sel2(allLectures), Tup.sel2(allTutorials) 
        yearSession   = buildSession Tup.sel3(allLectures), Tup.sel3(allTutorials) 
    if null sqlCourse
    then return emptyCourse
    else return (buildCourse fallSession springSession yearSession (entityVal $ head sqlCourse))