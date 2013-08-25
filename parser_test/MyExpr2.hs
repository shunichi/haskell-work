module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative hiding ( (<|>), many )

data Expr = Nat Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

prog :: Parser Expr
prog = expr <* eof

expr :: Parser Expr
expr = buildExpressionParser table term

{-
  	演算子のリストのリスト
	優先度の高い順

  	Infix parser assoc

    	parser 演算子文字列を消費して、演算子の関数を出力するパーサー
  		assoc  結合性
-}
table = [ [binary "*" Mul AssocLeft, binary "/" Div AssocLeft ]
        , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft ] ]
         where
             binary name fun assoc = Infix (do{ string name; return fun}) assoc

term :: Parser Expr
term = parens expr <|> nat

parens :: Parser Expr -> Parser Expr
parens exp = char '(' *> exp <* char ')'

nat :: Parser Expr
nat = Nat . read <$> many1 digit
