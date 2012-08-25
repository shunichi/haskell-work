
divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator = map f
    where
        f 0 = Nothing
        f denom = Just (numerator `div` denom)
