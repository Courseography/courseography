{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

{-|
This is a small module that contains a single enumeration type.
Note that we can't include this in "Database.Tables" because of
a restriction on Template Haskell.
-}
module Database.DataType where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson

data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq, Generic)
derivePersistField "ShapeType"

instance ToJSON ShapeType
instance FromJSON ShapeType
