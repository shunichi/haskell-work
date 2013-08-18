
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' _ acc [] = acc
myFoldl' f acc (x:xs) =
    let acc' = (f acc x)
    in acc' `seq` myFoldl f acc' xs

myFoldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
myFoldM _ acc [] = return acc
myFoldM f acc (x:xs) = do
    acc' <- (f acc x)
    myFoldM f acc' xs

myFoldrM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
myFoldrM _ acc [] = return acc
myFoldrM f acc (x:xs) = do
    acc' <- myFoldrM f acc xs
    f x acc'

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = myFoldr (\x ys -> if f x then x:ys else ys) [] xs

myFilterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
myFilterM _ [] = return []
myFilterM f (x:xs) = do
    b <- f x
    xs' <- myFilterM f xs
    case b of
        True  -> return $ x : xs'
        False -> return $ xs'
