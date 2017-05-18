import Data.Text.Lazy
import Data.GraphViz
import Data.Graph.Inductive.Example
import Data.GraphViz.Printing

main = putStrLn $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams clr479 