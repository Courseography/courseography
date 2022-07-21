module DynamicGraphs.GraphNodeUtils
    ( stringifyModAnd
    , formatModOr
    , concatModOr
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
stringifyModAnd :: Float -> [Modifier] -> String
stringifyModAnd creds modifiers = let
    dept = maybeHead [x | Department x <- modifiers]
    deptFormatted = case dept of
        Nothing -> ""
        Just x -> ' ' : x
    depts = maybeHead [xs | ModOr xs@((Department _):_) <- modifiers]
    deptsFormatted = case depts of
        Nothing -> ""
        Just xs -> ' ' : concatModOr xs
    level = maybeHead [x | Level x <- modifiers]
    levelFormatted = case level of
        Nothing -> ""
        Just x -> " at the " ++ x ++ " level"
    levels = maybeHead [xs | ModOr xs@((Level _):_) <- modifiers]
    levelsFormatted = case levels of
        Nothing -> ""
        Just xs -> " at the " ++ concatModOr xs ++ " level"
    raw = maybeHead [x | Requirement (Raw x) <- modifiers]
    rawFormatted = case raw of
        Nothing -> ""
        Just x -> " from " ++ x
    raws = maybeHead [xs | ModOr xs@((Requirement (Raw _)):_) <- modifiers]
    rawsFormatted = case raws of
        Nothing -> ""
        Just xs -> " from " ++ concatModOr xs

    in show creds ++ deptFormatted ++ deptsFormatted ++ " FCEs"
        ++ levelFormatted ++ levelsFormatted ++ rawFormatted ++ rawsFormatted

-- | Formats a ModOr into Fces string
-- | Assumes all modifiers in the ModOr have the same constructor
formatModOr :: Float -> [Modifier] -> String
formatModOr creds mods@((Department _):_) = show creds ++ " " ++ concatModOr mods ++ " FCEs"
formatModOr creds mods@((Level _):_) = show creds ++ " FCEs at the " ++ concatModOr mods ++ " level"
formatModOr creds mods@((Requirement (Raw _)):_) = show creds ++ " FCEs from " ++ concatModOr mods
formatModOr _ _ = "" -- we should never get here

-- | Joins a list of modifiers in ModOr together with a "/" or "or"
-- | Assumes all modifiers in the list have the same constructor
concatModOr :: [Modifier] -> String
concatModOr [Level x] = x
concatModOr ((Level x):xs) = x ++ "/" ++ concatModOr xs
concatModOr [Department x] = x
concatModOr ((Department x):xs) = x ++ "/" ++ concatModOr xs
concatModOr [Requirement (Raw x)] = x
concatModOr ((Requirement (Raw x)):xs) = x ++ " or " ++ concatModOr xs
concatModOr _ = "" -- we should never get here

-- | Returns Just the first element of the given list, or Nothing if the list is empty
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

paddingSpaces :: Int -> [Char]
paddingSpaces n = Prelude.replicate n ' '
