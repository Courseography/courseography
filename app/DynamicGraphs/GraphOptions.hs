import Data.Aeson ((.:?), (.!=), FromJSON(parseJSON), withObject)

data GraphOptions =
    GraphOption { courses :: [T.Text],      --courses to create a graph for
                  taken :: [T.Text],        -- courses to exclude from graph
                  departments :: [T.Text],  -- department prefixes to include
                  excludedDepth :: Int,     -- depth to recurse on courses from excluded departments
                  maxDepth :: Int,          --total recursive depth to recurse on (depth of the overall graph)
                  courseNumPrefix :: [Int], -- filter based on course number (most useful for filtering based on year)
                  distrib :: [T.Text],      -- distribution to include: like "artsci", or "engineering"
                  location :: [T.Text],     -- location of courses to include: like "st_george", or "scarborough"
                  noRaws :: Bool,           -- False to include nodes which are raw values
                  noGrades :: Bool          -- False to include grade nodes
                } deriving (Show, Generic)

instance FromJSON GraphOptions where
  parseJSON = withObject "Expected Object for GraphOptions" $ \o -> do
    courses <- o .:? "courses" .!= []
    taken <- o .:? "taken" .!= []
    departments <- o .:? "departments" .!= []
    excludedDepth <- o .:? "excludedDepth" .!= 0
    maxDepth <- o .:? "maxDepth" .!= -1
    courseNumPrefix <- o .:? "courseNumPrefix" .!= []
    distrib <- o .:? "distribution" .!= []
    location <- o .:? "location" .!= []
    noRaws <- o .:? "noRaws" .!= False
    noGrades <- o .:? "noGrades" .!= False
    return $ GraphOptions courses taken departments excludedDepth maxDepth courseNumPrefix distrib location noRaws noGrades
