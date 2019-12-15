module Eval where

import Data.Text (append)

import Model

eval :: Expr -> LoxResult
eval e = case e of
    Literal l -> Right l
    Binary e1 o e2 -> evalBinary e1 o e2
    Unary o e -> evalUnary o e
    Grouping e -> eval e

evalUnary :: UnaryOperator -> Expr -> LoxResult
evalUnary o e = do
    a <- eval e
    case o of
        UnaryMinus -> unaryNegate a
        Bang -> unaryNot a

evalBinary :: Expr -> Operator -> Expr -> LoxResult
evalBinary e1 o e2 = do
    a <- eval e1
    b <- eval e2
    case o of
        BangEqual -> Right . BoolLiteral $ a /= b
        Equal -> Right . BoolLiteral $ a == b
        EqualEqual -> Right . BoolLiteral $ a == b
        Greater -> Right . BoolLiteral $ a > b
        GreaterEqual -> Right . BoolLiteral $ a >= b
        Less -> Right . BoolLiteral $ a < b
        LessEqual -> Right . BoolLiteral $ a <= b
        Slash -> divide a b
        Star -> multiply a b
        Plus -> plus a b
        Minus -> minus a b

divide :: Literal -> Literal -> LoxResult
divide a b = case (a, b) of
    (NumberLiteral n1, NumberLiteral n2) ->
        Right $ NumberLiteral $ n1 / n2
    _ -> Left ("Cannot divide " ++ show a ++ " by " ++ show b)

multiply :: Literal -> Literal -> LoxResult
multiply a b = case (a, b) of
    (NumberLiteral n1, NumberLiteral n2) ->
        Right $ NumberLiteral $ n1 * n2
    _ -> Left ("Cannot multiply " ++ show a ++ " by " ++ show b)

plus :: Literal -> Literal -> LoxResult
plus a b = case (a, b) of
    (NumberLiteral n1, NumberLiteral n2) ->
        Right $ NumberLiteral $ n1 + n2
    (StringLiteral s1, StringLiteral s2) ->
        Right $ StringLiteral $ s1 `append` s2
    _ -> Left ("Cannot plus " ++ show a ++ " by " ++ show b)

minus :: Literal -> Literal -> LoxResult
minus a b = case (a, b) of
    (NumberLiteral n1, NumberLiteral n2) ->
        Right $ NumberLiteral $ n1 - n2
    _ -> Left ("Cannot minus " ++ show a ++ " by " ++ show b)

unaryNegate :: Literal -> LoxResult
unaryNegate a = case a of
    NumberLiteral n -> Right $ NumberLiteral $ -n
    _ -> Left ("Cannot negate " ++ show a)

unaryNot :: Literal -> LoxResult
unaryNot a = case a of
    BoolLiteral b -> Right $ BoolLiteral $ not b
    _ -> Left ("Cannot logically invert " ++ show a)
