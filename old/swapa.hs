
main = do cs <- getContents
          putStr $ swapa cs

swapa cs = map f cs
  where
    f 'a' = 'A'
    f 'A' = 'a' 
    f c   = c
