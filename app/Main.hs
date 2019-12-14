module Main where

import Text.Megaparsec
import qualified Data.Text.IO as T

import Cli
import Repl
import Lexer

main :: IO ()
main = do
    res <- runCli
    case res of
        Repl -> repl
        FileInput f -> do
            src <- T.readFile f
            parseTest scanTokens src