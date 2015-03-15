{-# LANGUAGE EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings,
             DeriveGeneric,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}
module Database.DataType where

import Database.Persist.TH
import Database.Persist
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Control.Monad
import Control.Applicative

data ShapeType = BoolNode | Node | Hybrid | Region
 deriving (Show, Read, Eq)
derivePersistField "ShapeType"
