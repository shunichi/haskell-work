-- 空行とコメント行以外を数える

module Main (main, countCodeLines, isCodeLine, isWhiteSpace, countCodeLinesOfFile) where

import System.Environment

main = printCodeLinesMain

printArgs = do args <- getArgs
               putStrLn $ " " ++ (show args) ++ "\n"

printCodeLinesMain = do args <- getArgs
                        str <- makeListString args
                        putStr str

makeListString :: [String] -> IO String
makeListString fileNames = do nameAndLines <- mapM countCodeLinesOfFile fileNames
--                              xs <- return $ map catNameLine nameAndLines
--                              str <- return $ concat xs
                              str <- return $ concatMap catNameLine nameAndLines
                              sumstr <- return $ show $ foldl (+) 0 $ snd (unzip nameAndLines)
                              return $ str ++ "sum = " ++ sumstr ++ "\n"
    where
        catNameLine (name, line) = name ++ " " ++ (show line) ++ "\n"

-- ファイル中の空行やコメント行以外の行数をカウントする
countCodeLinesOfFile :: String -> IO (String, Int)
countCodeLinesOfFile fileName = do cs <- readFile fileName
                                   return (fileName, (countCodeLines cs))

-- 文字列中の空行やコメント行以外の行数をカウントする
countCodeLines str = length $ filter isCodeLine (lines str)

isCodeLine str = case (span isWhiteSpace str) of
    (_,"")          -> False
    (_,('-':'-':_)) -> False
    _               -> True

isWhiteSpace ' '  = True
isWhiteSpace '\t' = True
isWhiteSpace _    = False

