module Main where

unit :: a -> [a]
unit x = [x]

bind :: [a] -> (a -> [b]) -> [b]
-- bind m f = concatMap f m
bind m f = foldr ((++) . f) [] m

foo = [1, 2, 3] `bind` (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

bar = mysequence [[1,2],[3,4]]

mysequence :: [[a]] -> [[a]] -- [m a] -> m [a]
mysequence [] = [[]]
mysequence (m:ms) = m `bind` (\x -> (mysequence ms) `bind` (\xs -> unit (x:xs)))

{-
mysequence2 :: (Monad m) => [m a] -> m [a]
mysequence2 [] = return []
mysequence2 (m:ms) = do
    x <- m
    xs <- mysequence2 ms
    return (x:xs)
-}

mysequence2 :: (Monad m) => [m a] -> m [a]
mysequence2 [] = return []
mysequence2 (m:ms) = m >>= (\x -> (mysequence2 ms) >>= (\xs -> return (x:xs)))

{-
  mysequence2 [[1,2],[3,4]]
  [1,2] >>= (\x -> (mysequence2 [[3,4]]) >>= (\xs -> return (x:xs)))
  concat [(mysequence2 [[3,4]]) >>= (\xs -> return (1:xs)), (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [(mysequence2 [[3,4]]) >>= (\xs -> return (1:xs)), (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [concat $ map (\xs -> return (1:xs)) (mysequence2 [[3,4]]), (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [concat $ map (\xs -> return (1:xs)) [[3], [4]], (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [concat $ [return [1,3], return [1,4]], (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [concat [[[1,3]], [[1,4]]], (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [ [[1,3], [1,4]], (mysequence2 [[3,4]]) >>= (\xs -> return (2:xs))]
  concat [ [[1,3], [1,4]], [[3],[4]] >>= (\xs -> return (2:xs))]

  mysequence2 [[3,4]]
  mysequence2 ([3,4]:[])
  [3,4] >>= (\x -> (mysequence2 []) >>= (\xs -> return (x:xs)))
  [3,4] >>= (\x -> [[]] >>= (\xs -> return (x:xs)))
  concat $ map (\x -> [[]] >>= (\xs -> return (x:xs))) [3,4]
  concat [ [[]] >>= (\xs -> return (3:xs)), [[]] >>= (\xs -> return (4:xs)) ]
  concat [ concat $ map (\xs -> return (3:xs)) [[]], concat $ map (\xs -> return (4:xs)) [[]] ]
  concat [ concat [(\xs -> return (3:xs))[]], concat [(\xs -> return (4:xs))[]] ]
  concat [ concat [return (3:[])], concat [return (4:[])] ]
  concat [ concat [return [3]], concat [return [4]] ]
  concat [ concat [[[3]]], concat [[[4]]] ]
  concat [ [[3]], [[4]] ]
  [ [3], [4] ]

-}
  
{-
concat $ map (\x -> [4, 5, 6] `bind` (\y -> [(x,y)])) [1, 2, 3]
concat $ map f [1, 2, 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [f 1, f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [[4, 5, 6] `bind` (\y -> [(1,y)]), f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [concat $ map (\y -> [(1,y)]) [4, 5, 6], f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [concat $ [(\y -> [(1,y)])4, (\y -> [(1,y)])5, (\y -> [(1,y)])6], f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
  concat $ [concat $ [[(1,4)], [(1,5)], [(1,6)]], f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [[(1,4), (1,5), (1,6)], f 2, f 3] where f = (\x -> [4, 5, 6] `bind` (\y -> [(x,y)]))
concat $ [[(1,4), (1,5), (1,6)], [(2,4), (2,5), (2,6)] , [(3,4), (3,5), (3,6)]]
[(1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)]
-}
