{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Data.Void (Void)
import Data.Text (Text, unpack)
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Char (space1, char, string)

import Model (TokenType(..))

import qualified Data.HashMap.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol' :: Text -> Parser Text
symbol' = L.symbol sc

symbol :: TokenType -> Text -> Parser TokenType
symbol t s = t <$ symbol' s

symbolLookAhead :: TokenType -> Char -> TokenType -> Char -> Parser TokenType
symbolLookAhead t1 c1 t2 c2 = lexeme $ do
    char c1
    res <- optional $ char c2
    case res of
        Just c -> return t2
        Nothing -> return t1

stringLiteral :: Parser TokenType
stringLiteral = lexeme $ do
    char '"'
    res <- takeWhileP (Just "String literal") ('"' /=)
    char '"'
    return $ StringLiteral res

number :: Parser TokenType
number = Number
    <$> lexeme ((try L.float) <|> (fromInteger <$> L.decimal))

leftParen = symbol LeftParen "("
rightParen = symbol RightParen ")"
leftBrace = symbol LeftBrace "{"
rightBrace = symbol RightBrace "}"
comma = symbol Comma ","
dot = symbol Dot "."
minus = symbol Minus "-"
plus = symbol Plus "+"
semicolon = symbol SemiColon ";"
star = symbol Star "*"
bangBangEqual = symbolLookAhead Bang '!' BangEqual '='
equalEqualEqual = symbolLookAhead Equal '=' EqualEqual '='
lessLessEqual = symbolLookAhead Less '<' LessEqual '='
greaterGreaterEqual = symbolLookAhead Greater '>' GreaterEqual '='
slash = symbol Slash "/"

keywords :: Map.HashMap Text TokenType
keywords = Map.fromList $
    [ ("and", And)
    , ("or", Or)
    , ("class", Class)
    , ("super", Super)
    , ("if", If)
    , ("else", Else)
    , ("true", True')
    , ("false", False')
    , ("for", For)
    , ("while", While)
    , ("fun", Fun)
    , ("return", Return)
    , ("print", Print)
    , ("this", This)
    , ("var", Var)
    , ("nil", Nil)
    , ("eof", Eof)
    ]

identifierOrKeyword :: Parser TokenType
identifierOrKeyword = lexeme $ do
    text <- takeWhile1P (Just "Identifier or keyword") isAlphaNum
    return $ case Map.lookup text keywords of
        Just a -> a
        Nothing -> Identifier text

scanToken :: Parser TokenType
scanToken = choice
    [ leftParen
    , rightParen
    , leftBrace
    , rightBrace
    , comma
    , dot
    , plus
    , minus
    , semicolon
    , star
    , bangBangEqual
    , equalEqualEqual
    , lessLessEqual
    , greaterGreaterEqual
    , slash
    , stringLiteral
    , number
    , identifierOrKeyword
    ]

scanTokens :: Parser [TokenType]
scanTokens = many scanToken
