{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

{-|
    Module      : Database.DataType
    Description : Contains a single enumeration type, ShapeType.

This is a small module that contains a single enumeration type.
Note that we can't include this in "Database.Tables" because of
a restriction on Template Haskell.
-}
module Database.DataType where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

-- | Defines a datatype of a shape, used in Shape json table.
data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq, Generic)

-- | Results from [derivePersistField](https://hackage.haskell.org/package/persistent-template-2.5.1.6/docs/Database-Persist-TH.html#v:derivePersistField)
-- call, as does PersistField, most importantly, it allows the data type to be
-- a column in the database.
derivePersistField "ShapeType"

-- | Results from call of [ToJSON](https://hackage.haskell.org/package/aeson-1.1.0.0/docs/Data-Aeson.html#t:ToJSON)
-- .
instance ToJSON ShapeType

-- | Results from call of [FromJSON](https://hackage.haskell.org/package/aeson-1.1.0.0/docs/Data-Aeson.html#t:FromJSON)
-- .
instance FromJSON ShapeType

data PostType = Specialist | Major | Minor | Focus | Certificate | Other
 deriving (Show, Read, Eq, Generic)
derivePersistField "PostType"

instance ToJSON PostType
instance FromJSON PostType
