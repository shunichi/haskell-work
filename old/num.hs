
main = do cs <- getContents
          putStr $ unlines $ number $ lines cs

number ss = map concatNum $ zip [1..] ss
  where concatNum (n, s) = (show n) ++ " " ++ s


