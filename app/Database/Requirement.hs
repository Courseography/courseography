{-|
    Module      : Database.Requirement
    Description : Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}

module Database.Requirement
( Req(..) ) where

data Req = NONE
         | J String String
         | AND [Req]
         | OR [Req]
         | FCES Float Req
         | GRADE String Req
         | PROGRAM String
         | RAW String deriving (Eq, Show)
