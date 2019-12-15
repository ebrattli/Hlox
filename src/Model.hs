{-# LANGUAGE GADTs #-}
module Model where

import Data.Text

data Expr
    = Literal Literal
    | Binary Expr Operator Expr
    | Unary UnaryOperator Expr
    | Grouping Expr
instance Show Expr where
    show (Literal l) = show l
    show (Binary e1 o e2) = "(" ++ show e1 ++ ") " ++ show o ++ " (" ++ show e2 ++ ")"
    show (Unary o e) = show o ++ show e
    show (Grouping e) = "(" ++ show e ++ ")"

data Literal
    = Identifier Text
    | StringLiteral Text
    | NumberLiteral Double
    | BoolLiteral Bool
    | Nil
instance Show Literal where
    show (Identifier t) = unpack t
    show (StringLiteral t) = "\"" ++ unpack t ++ "\""
    show (NumberLiteral n) = show n
    show (BoolLiteral b) = show b
    show Nil = "nil"

data Operator
    = BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Slash
    | Star
    | Plus
    | Minus
instance Show Operator where
    show BangEqual = "!="
    show Equal = "="
    show EqualEqual = "=="
    show Greater = ">"
    show GreaterEqual = ">="
    show Less = "<"
    show LessEqual = "<="
    show Slash = "/"
    show Star = "*"
    show Plus = "+"
    show Minus = "-"

data UnaryOperator
    = UnaryMinus
    | Bang
instance Show UnaryOperator where
    show UnaryMinus = "-"
    show Bang = "!"

data TokenType
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | SemiColon
    | And
    | Or
    | Class
    | Super
    | If
    | Else
    | For
    | While
    | Fun
    | Return
    | Print
    | This
    | Var
    | Eof
    deriving (Show)

data Token = Token
    { tokenType :: TokenType
    , lexeme :: String
    , line :: Integer
    }

instance Show Token where
    show token = (show . tokenType) token ++ " " ++ lexeme token
