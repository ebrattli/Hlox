{-# LANGUAGE GADTs #-}
module Model where

import Data.Text

data TokenType
    -- Single char tokens
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Plus
    | Minus
    | SemiColon
    | Slash
    | Star
    -- Logical operator tokens
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    -- Literals
    | Identifier Text
    | StringLiteral Text
    | Number Double
    -- Keywords
    | And
    | Or
    | Class
    | Super
    | If
    | Else
    | True'
    | False'
    | For
    | While
    | Fun
    | Return
    | Print
    | This
    | Var
    | Nil
    | Eof
    deriving (Show)


data Token = Token
    { tokenType :: TokenType
    , lexeme :: String
    , line :: Integer
    }

instance Show Token where
    show token = (show . tokenType) token ++ " " ++ lexeme token
