{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import Text.Parsec.Pos
import Control.Applicative ((<*>), (*>), (<*), (<$>))
import Control.Monad.Trans
import Utility
import Token

type Parser = Parsec String ()

data CgProgram = CgProgDefun { defun :: CgDefun }
               | CgProgDefStruct { defStruct :: CgDefStruct }
               | CgProgDefVar { defVar :: CgDefVar }
               deriving (Show)


{-
	int a;
	float4x4 m[3] : semantic;
-}

data CgVarDim = CgScalar | CgArray [Int]
              deriving(Show)
data CgDefVar = CgDefVar { cgVarTypeName :: String, cgVarname :: String, cgVarDim :: CgVarDim, cgVarSemantic :: String }
              deriving(Show)

pDefVar :: Parser CgDefVar
pDefVar = do
    tName <- identifier
    iName <- identifier
    dim   <- pArrayBrackets
    sName <- identifier <|> (return "")
    (symbol ";")
    return $ CgDefVar tName iName dim sName

pArrayBrackets :: Parser CgVarDim
pArrayBrackets = lexeme (f <$> (many $ brackets natural))
	where
    	f [] = CgScalar
        f xs = CgArray $ map fromIntegral xs

data CgDefun = CgDefun { cgFuncName :: String }
    deriving (Show)
data CgDefStruct = CgDefStruct { name :: String }
    deriving (Show)

struct = do
    string "struct" >> spaces
    str <- ident
    spaces >> char '{' >> spaces
    char '}' >> spaces >> char ';'
    return $ CgDefStruct str


struct2 = CgDefStruct <$> (h *> ident <* t)
    where
        h = string "struct" >> spaces1
        t = spaces >> char '{' >> spaces >> char '}' >> spaces >> char ';'

ident :: Parser String
ident = do
    c <- identHead
    str <- many identTail
    return (c:str)

identHead :: Parser Char
identHead = choice [char '_', letter]

identTail :: Parser Char
identTail = choice [char '_', alphaNum]

cgTypeName :: Parser String
cgTypeName = choice $ map string
    [ "float", "float1", "float2", "float3", "float4",
      "int", "int1", "int2", "int3", "int3",
      "float2x2", "float2x3", "float2x4",
      "float3x2", "float3x3", "float3x4",
      "float4x2", "float4x3", "float4x4" ]

