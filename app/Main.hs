module Main where

import qualified Data.Text.IO as T

import Cli
import Repl

main :: IO ()
main = do
    res <- runCli
    case res of
        Repl -> repl
        FileInput f -> do
            src <- T.readFile f
            print src