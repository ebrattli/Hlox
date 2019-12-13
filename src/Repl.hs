module Repl
    ( repl
    )
where

import System.IO
import System.Exit

eval :: String -> IO ()
eval s =
    if s == "q" || s == "quit"
        then exitSuccess
        else putStrLn s

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    eval input
    repl
