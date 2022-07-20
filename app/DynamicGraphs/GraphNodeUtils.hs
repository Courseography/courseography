module DynamicGraphs.GraphNodeUtils
    ( stringifyModand
    , formatModor
    , concatModor
    , maybeHead
    , paddingSpaces
    ) where

import Database.Requirement (Modifier (..), Req (..))

-- | Converts the given number of credits and list of modifiers into a string
-- | in readable English
-- | Assumes each modifier constructor appears in modifiers at most once
-- | The ModOr constructor may appear more than once, but each occurrence
-- | of ModOr contains exactly one constructor for all its elements
-- | and such constructor does not appear anywhere else in ModAnd
stringifyModand :: Float -> [Modifier] -> String
stringifyModand creds modifiers = let
    dept = maybeHead [x | Department x <- modifiers]
    deptFormatted = case dept of
        Nothing -> ""
        Just x -> ' ' : x
    depts = maybeHead [xs | ModOr xs@((Department _):_) <- modifiers]
    deptsFormatted = case depts of
        Nothing -> ""
        Just xs -> ' ' : concatModor xs
    level = maybeHead [x | Level x <- modifiers]
    levelFormatted = case level of
        Nothing -> ""
        Just x -> " at the " ++ x ++ " level"
    levels = maybeHead [xs | ModOr xs@((Level _):_) <- modifiers]
    levelsFormatted = case levels of
        Nothing -> ""
        Just xs -> " at the " ++ concatModor xs ++ " level"
    raw = maybeHead [x | Requirement (Raw x) <- modifiers]
    rawFormatted = case raw of
        Nothing -> ""
        Just x -> " from " ++ x
    raws = maybeHead [xs | ModOr xs@((Requirement (Raw _)):_) <- modifiers]
    rawsFormatted = case raws of
        Nothing -> ""
        Just xs -> " from " ++ concatModor xs

    in show creds ++ deptFormatted ++ deptsFormatted ++ " FCEs"
        ++ levelFormatted ++ levelsFormatted ++ rawFormatted ++ rawsFormatted

-- | Formats a ModOr into FCEs string
-- | Assumes all modifiers in the ModOr have the same constructor
formatModor :: Float -> [Modifier] -> String
formatModor creds mods@((Department _):_) = show creds ++ " " ++ concatModor mods ++ " FCEs"
formatModor creds mods@((Level _):_) = show creds ++ " FCEs at the " ++ concatModor mods ++ " level"
formatModor creds mods@((Requirement (Raw _)):_) = show creds ++ " FCEs from " ++ concatModor mods
formatModor _ _ = "" -- we should never get here

-- | Joins a list of modifiers in ModOr together with a "/" or "or"
-- | Assumes all modifiers in the list have the same constructor
concatModor :: [Modifier] -> String
concatModor [Level x] = x
concatModor ((Level x):xs) = x ++ "/" ++ concatModor xs
concatModor [Department x] = x
concatModor ((Department x):xs) = x ++ "/" ++ concatModor xs
concatModor [Requirement (Raw x)] = x
concatModor ((Requirement (Raw x)):xs) = x ++ " or " ++ concatModor xs
concatModor _ = "" -- we should never get here

-- | Returns Just the first element of the given list, or Nothing if the list is empty
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

paddingSpaces :: Int -> [Char]
paddingSpaces n = Prelude.replicate n ' '
