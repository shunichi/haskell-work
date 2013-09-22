module Main where

import Text.Parsec
import Control.Applicative ((<*>), (*>), (<*), (<$>))
-- import Control.Monad.Trans
import Utility
import Token

type Parser = Parsec String ()

data CgVarDim = CgScalar | CgArray [Int]
              deriving(Show)
data CgDefVar = CgDefVar { cgVarTypeName :: String, cgVarName :: String, cgVarDim :: CgVarDim, cgVarSemantic :: String }
              deriving(Show)
data CgDefStruct = CgDefStruct { name :: String, members :: [CgDefVar] }
    deriving (Show)
data CgDefun = CgDefun { cgFuncName :: String }
    deriving (Show)

pDefVar :: Parser CgDefVar
pDefVar = do
    typeName <- identifier
    ident    <- identifier
    dim      <- pArrayBrackets
    semantic <- (symbol ":" >> identifier) <|> (return "")
    symbol ";"
    return (CgDefVar typeName ident dim semantic)

pArrayBrackets :: Parser CgVarDim
pArrayBrackets = lexeme (f <$> (many $ brackets natural))
	where
    	f [] = CgScalar
        f xs = CgArray $ map fromIntegral xs

pDefStruct = do
    reserved "struct"
    ident <- identifier
    membs <- between (symbol "{") (symbol "}") (many pDefVar)
    symbol ";"
    return $ CgDefStruct ident membs
