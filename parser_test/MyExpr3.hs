module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import Control.Applicative hiding ( (<|>), many )

-- ==================================================================
--  TokenParser の定義
-- ==================================================================

{-
  Javaスタイルのコメントや空白文字を無視してくれるトークンパーサー
-}
lexer :: P.TokenParser ()
lexer = P.makeTokenParser javaStyle

whiteSpace = P.whiteSpace lexer   -- 空白文パーサー
lexeme     = P.lexeme lexer       -- 後続のスペースを飛ばす
natural    = P.natural lexer      -- 数値パーサー
parens     = P.parens lexer       -- 括弧でかこむ
reservedOp = P.reservedOp lexer   -- 演算子パーサーを作る関数

-- ==================================================================
--  パーサー本体
-- ==================================================================
data Expr = Nat Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

prog :: Parser Expr
prog = expr <* eof

expr :: Parser Expr
expr = buildExpressionParser table term

table = [ [binary "*" Mul AssocLeft, binary "/" Div AssocLeft ]
        , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft ] ]
         where
             binary name fun assoc = Infix (do{ reservedOp name; return fun}) assoc

term :: Parser Expr
term = parens expr <|> (Nat <$> natural)
