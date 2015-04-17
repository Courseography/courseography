{-# LANGUAGE TemplateHaskell #-}
module Database.DataType where

import Database.Persist.TH

data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq)
derivePersistField "ShapeType"
