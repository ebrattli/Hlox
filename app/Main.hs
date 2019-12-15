module Main where

import Text.Megaparsec
import qualified Data.Text.IO as T

import Cli
import Repl
import Parser

main :: IO ()
main = do
    res <- runCli
    case res of
        Repl -> repl
        FileInput f -> do
            src <- readFile f
            print $ parseLox src
