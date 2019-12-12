module Main where

import Cli

main :: IO ()
main = do
    res <- runCli
    case res of
        Repl -> print "repling"
        FileInput f -> print $ "executing file: " ++ f