safeRoot :: Float -> Maybe Float
safeRoot x 
     | x < 0     = Nothing
     | otherwise = Just (sqrt x)

safeHead :: [a] -> Maybe a
safeHead x
    | length x == 0 = Nothing
    | otherwise = Just $ head x

getValue :: Maybe a -> a -> a
getValue a b = case a of
                   Nothing -> b
                   Just c -> c

filterMaybe :: [Maybe a] -> [a]
filterMaybe lst = foldl (\x y -> case y of 
                                 Nothing -> x
                                 Just d -> x ++ [d]) [] lst

