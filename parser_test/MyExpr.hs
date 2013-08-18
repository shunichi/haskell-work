module Main (Expr, nat) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ( (<|>), many )

data Expr = Nat Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)

nat :: Parser Expr
nat = Nat . read <$> many1 digit

{-
	右再帰
  	expr := term '+' expr | term
  	term := factor '*' term | factor
    factor := '(' expr ')' | nat
-}

expr :: Parser Expr
expr = try ( Add <$> term <* char '+' <*> expr ) <|> term

term :: Parser Expr
term = try ( Mul <$> factor <* char '*' <*> term ) <|> factor

factor :: Parser Expr
factor = pexpr <|> nat
 where
     pexpr = char '(' *> expr <* char ')'

{-
	左再帰
    expr := expr '+' term | term
    term := term '*' factor | factor
    factor := '(' expr ')' | nat

    左再帰除去
    expr := term ('+' term | e)*
    term := factor ('*' factor | e)*
    factor := '(' expr ')' | nat
-}

exprL :: Parser Expr
exprL = foldl Add <$> termL <*> many (char '+' *> termL)

termL :: Parser Expr
termL = foldl Mul <$> factorL <*> many (char '*' *> factorL)

factorL :: Parser Expr
factorL = pexpr <|> nat
 where
     pexpr = char '(' *> exprL <* char ')'
