{-# LANGUAGE FlexibleContexts #-}

module Main where
import Text.Parsec
import Text.Parsec.Pos
import Control.Applicative ((<*>), (*>), (<*), (<$>))
import Control.Monad.Trans


type Parser = Parsec String ()

data CgStruct = CgStruct { name :: String }
    deriving (Show)

struct = do
    string "struct" >> spaces
    str <- ident
    spaces >> char '{' >> spaces
    char '}' >> spaces >> char ';'
    return $ CgStruct str


struct2 = CgStruct <$> (h *> ident <* t)
    where
        h = string "struct" >> spaces
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
