{-# LANGUAGE TemplateHaskell #-}
module Database.DataType where

import Database.Persist.TH
import Database.Persist

data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq)
derivePersistField "ShapeType"
