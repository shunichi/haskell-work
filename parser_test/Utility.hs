{-# LANGUAGE FlexibleContexts #-}

module Utility (spaces1) where

import Text.Parsec

spaces1 :: (Stream s m Char) => ParsecT s u m ()
spaces1 = skipMany1 space <?> "white space"

