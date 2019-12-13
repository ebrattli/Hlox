{-# LANGUAGE GADTs #-}

module Model where

data Literal
    = Identifier String
    | Str String
    | Number Integer

data TokenType =
    -- Single char tokens
    LeftParen | RightParen
    | LeftBrace | RightBrace
    | Comma | Dot
    | Plus | Minus | SemiColon
    | Slash | Star
    -- Logical operator tokens
    | Bang | BangEquals
    | Equal | EqualEqual
    | Greater | GreaterEqual
    | Less | LessEqual
    -- Literals
    | Literal
    -- Keywords
    | AND | OR
    | CLASS | SUPER
    | IF | ELSE
    | TRUE | FALSE
    | FOR | WHILE
    | FUN | RETURN
    | PRINT | THIS
    | VAR | NIL
    | EOF
    deriving (Show)


data Token = Token
    { tokenType :: TokenType
    , lexeme :: String
    , line :: Integer
    }

instance Show Token where
    show token = (show . tokenType) token ++ " " ++ lexeme token
