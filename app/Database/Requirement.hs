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

data Req = None
         | J String String
         | ReqAnd [Req]
         | ReqOr [Req]
         | Fces Float Modifier
         | Grade String Req
         | Gpa Float String
         | Program String
         | Raw String deriving (Eq, Show)

data Modifier = Department String
              | Level String
              | ModAnd [Modifier]
              | ModOr [Modifier]
              | Requirement Req deriving (Eq, Show)
