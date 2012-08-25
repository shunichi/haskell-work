import Control.Monad (forM)

divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ [] = Just []
divBy _ (0:_) = Nothing
divBy numerator (denom:xs) =
    case divBy numerator xs of
        Nothing -> Nothing
        Just results -> Just ((numerator `div` denom) : results)

divBy2 :: Integral a => a -> [a] -> Maybe [a]
divBy2 n xs = forM xs $ \x -> case x of
    0 -> Nothing
    _ -> Just $ n `div` x
