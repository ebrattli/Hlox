module Repl
    ( repl
    )
where

import qualified Eval as E

import System.IO
import System.Exit

import Parser (parseLox)
import Model

eval :: String -> IO ()
eval s =
    if s == "q" || s == "quit"
        then exitSuccess
        else print $ (map E.eval) <$> (parseLox s)

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    eval input
    repl
