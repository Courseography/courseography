{-|
    Module      : Database.Requirement
    Description : Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}

module Database.Requirement (
    Req (..),
    Modifier (..)
    ) where

data Req = NONE
         | J String String
         | REQAND [Req]
         | REQOR [Req]
         | FCES Float Modifier
         | GRADE String Req
         | GPA Float String
         | PROGRAM String
         | RAW String deriving (Eq, Show)

data Modifier = DEPARTMENT String
              | LEVEL String
              | MODAND [Modifier]
              | REQUIREMENT Req deriving (Eq, Show)
