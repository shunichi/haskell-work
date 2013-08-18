{-# LANGUAGE FlexibleContexts #-}

module Token (identifier, natural, symbol, brackets, lexeme) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Control.Monad.Identity

cgStyle :: (Stream s m Char) => T.GenLanguageDef s u m
cgStyle = T.LanguageDef
	{ T.commentStart = "/*"
    , T.commentEnd   = "*/"
    , T.commentLine  = "//"
    , T.nestedComments = False
    , T.identStart     = letter <|> char '_'
    , T.identLetter	 = alphaNum <|> char '_'
    , T.opStart	 = T.opLetter cgStyle
    , T.opLetter	 = oneOf "!%&*+/<=>?^|-~"
    , T.reservedOpNames= []
    , T.reservedNames  = []
    , T.caseSensitive  = True }

lexer :: (Stream s m Char) => T.GenTokenParser s u m
lexer = T.makeTokenParser cgStyle

identifier :: (Stream s m Char) => ParsecT s u m String
identifier = T.identifier lexer

natural :: (Stream s m Char) => ParsecT s u m Integer
natural = T.natural lexer

symbol :: (Stream s m Char) => String -> ParsecT s u m String
symbol = T.symbol lexer

brackets :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
brackets = T.brackets lexer

lexeme :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
lexeme = T.lexeme lexer
