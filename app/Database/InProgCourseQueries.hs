--| UberGoal: go from T.Text -> [Entity Lecture] / [Entity Tutorial] without multiple DB queries.


-- | Step 1: single query for lecture or tutorial. Produces list of all 
-- | lectures/tutorials respectively, whcih are not separated by session



-- | Step 2: find a way to split these into separate lists.

-- | goal: refactor both below to something like
-- | splitSessions :: Entity => [a] -> ([a], [a], [a]) 

-- | Returns a 3-element tuple representing Fall, Spring, and Yearlong entity lists.
-- | problem: Data.Text vs String
splitSessionsL2 :: [Entity Lecture] -> ([Entity Lecture], [Entity Lecture], [Entity Lecture])
splitSessionsL lecturesList = 
    let fallLec = getBySessionL lecturesList "F"
    let springLec = getBySessionL lecturesList "S"
    let yearLec = getBySessionL lecturesList "Y"

    return (fallLec, springLec, yearLec)



    



-- nb: overloaded strings!

-- | Step 2.5: Helper function to separate lists.
-- | However, this is inefficient because it requires 3 separate calls to the [Entity] list.
-- | There is probably a way to go directly to a tuple of lists.

-- | goal: refactor both below to something like
-- | getBySession :: [EntityField a] -> [Entity a] -> [Entity a]

-- check out getDeptCourses line 189 for filter, etc

-- | Returns the matching lecture entities if sessions correspond with session code.
  -- bb: need Data.Text not string!
getBySessionL :: String -> [Entity Lecture] -> [Entity Lecture]
getBySessionL xs sessionStr = [ x | x <- xs, (LectureSession x) == sessionStr ] 
--let c = filter (\lec -> lectureSession lec == sessionStr) $ map entityVal xs

getBySessionT :: String -> [Entity Tutorial] -> [Entity Tutorial]
getBySessionT xs sessionStr = [ x | x <- xs, (TutorialSessis!`on x) == sessionStr ]


-- | Queries db for all matching records with lecture or tutorial code of this course
lectureQuery :: MonadIO m => T.Text -> ReaderT SqlBackend m a 
lectureQuery courseCode = selectList [LectureCode ==. courseCode] []

tutorialQuery :: MonadIO m => T.Text -> ReaderT SqlBackend m a 
tutorialQuery courseCode = selectList [TutorialCode ==. courseCode] []


-- | 
splitSessionsT :: [Entity Tutorial] -> ([Entity Tutorial], [Entity Tutorial], [Entity Tutorial])
splitSessionsT tutorialsList = 
let tutList = map entityVal tutorialsList
    return (fallTut, springTut, yearTut)
where
    fallTut = filter (\tut -> tutorialSession tut == "F") tutList
    springT = filter (\tut -> tutorialSession tut == "S") tutList
    yearTut = filter (\tut -> tutorialSession tut == "Y") tutList

splitSessionsL :: [Entity Lecture] -> ([Entity Lecture], [Entity Lecture], [Entity Lecture])
splitSessionsL lecturesList = 
  let lecList = map entityVal lecturesList
      return (fallLec, springLec, yearLec)
  where
      fallLec = filter (\lec -> lectureSession lec == "F") lecList
      springLec = filter (\lec -> lectureSession lec == "S") lecList
      yearLec = filter (\lec -> lectureSession lec == "Y") lecList
                
                
-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode ==. courseStr] [] 
    case sqlCourse of
      Nothing -> return emptyCourse
      Just course -> do
        (fallL, springL, yearL) <- splitSessionsL $ lectureQuery courseStr
        (fallT, springT, yearT) <- splitSessionsT $ lectureQuery courseStr
        
        let fallSession = buildSession fallL fallT
            springSession = buildSession springL springT
            yearSession = buildSession yearL yearT

        return (buildCourse fallSession springSession yearSession (entityVal $ head sqlCourse))
