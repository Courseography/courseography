{-# LANGUAGE TemplateHaskell #-}

{-|
This is a small module that contains a single enumeration type.
Note that we can't include this in "Database.Tables" because of
a restriction on Template Haskell.
-}
module Database.DataType where

import Database.Persist.TH

data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq)
derivePersistField "ShapeType"
