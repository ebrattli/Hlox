{-# LANGUAGE OverloadedStrings #-}
module Lexer () where

import Data.Void
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char

import Model

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

leftParen = const LeftParen <$> symbol "("
rightParen = symbol ")"

-- lexer :: String -> [Token]
-- lexer input =
