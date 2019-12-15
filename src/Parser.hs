{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Void (Void)
import Data.Text (Text, unpack)
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Char (space1, char, string)

import Model (TokenType(..), Operator(..), UnaryOperator(..), Expr(..), Literal(..))

import qualified Data.HashMap.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E

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

symbol :: a -> Text -> Parser a
symbol t s = t <$ symbol' s

symbolNotFollowedBy :: a -> Char -> Char -> Parser a
symbolNotFollowedBy t1 c1 c2 = try
    $ lexeme
    $ t1
    <$ char c1
    <* notFollowedBy (char c2)

stringLiteral :: Parser Literal
stringLiteral = lexeme $ do
    char '"'
    res <- takeWhileP (Just "String literal") ('"' /=)
    char '"'
    return $ StringLiteral res

numberLiteral :: Parser Literal
numberLiteral = NumberLiteral
    <$> lexeme ((try L.float) <|> (fromInteger <$> L.decimal))

leftParen = symbol LeftParen "("
rightParen = symbol RightParen ")"
leftBrace = symbol LeftBrace "{"
rightBrace = symbol RightBrace "}"
comma = symbol Comma ","
dot = symbol Dot "."
minus = symbol Minus "-"
unaryMinus = symbol UnaryMinus "-"
plus = symbol Plus "+"
semicolon = symbol SemiColon ";"
star = symbol Star "*"
bang = symbolNotFollowedBy Bang '!' '='
bangEqual = symbol BangEqual "!="
equal = symbolNotFollowedBy Equal '=' '='
equalEqual = symbol EqualEqual "=="
less = symbolNotFollowedBy Less '<' '='
lessEqual = symbol LessEqual "<="
greater = symbolNotFollowedBy Greater '>' '='
greaterEqual = symbol GreaterEqual ">="
slash = symbol Slash "/"

identifier :: Parser Text
identifier = lexeme $
    takeWhile1P (Just "Identifier or keyword") isAlphaNum

literalIdentifier :: Parser Literal
literalIdentifier = do
    val <- identifier
    case val of
        "true" -> pure $ BoolLiteral True
        "false" -> pure $ BoolLiteral False
        "nil" -> pure Nil
        a -> fail $ "Expected literal but got " ++ show a

literal :: Parser Expr
literal = Literal <$>
    (stringLiteral <|> numberLiteral <|> literalIdentifier)

grouping :: Parser Expr
grouping = do
    lexeme $ char '('
    e <- pExpr
    lexeme $ char ')'
    return e

unary :: Parser Expr
unary = do
    op <- bang <|> unaryMinus
    Unary op <$> expr

expr :: Parser Expr
expr = literal
    <|> grouping

binop :: Parser Operator -> E.Operator Parser Expr
binop op = E.InfixL (flip Binary <$> op)

prefix :: Parser UnaryOperator -> E.Operator Parser Expr
prefix op = E.Prefix (Unary <$> op)

operatorTable :: [[E.Operator Parser Expr]]
operatorTable =
    [ [ prefix bang
      , prefix unaryMinus
      ]
    , [ binop star
      , binop slash
      ]
    , [ binop plus
      , binop minus
      ]
    , [ binop less
      , binop lessEqual
      , binop greater
      , binop greaterEqual
      , binop equal
      , binop equalEqual
      ]
    ]

pExpr :: Parser Expr
pExpr = E.makeExprParser expr operatorTable

parseLox :: Parser [Expr]
parseLox = many $ do
    e <- pExpr
    semicolon
    pure e
