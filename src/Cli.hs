module Cli
    ( Input(..)
    , runCli
    )
where

import Options.Applicative
import Data.Semigroup ( (<>) )

data Input = FileInput String | Repl
    deriving Show

toInput :: Maybe String -> Input
toInput (Just s) = FileInput s
toInput Nothing  = Repl

fileInput :: Parser Input
fileInput =
    toInput
    <$> optional (argument str (help "tct"))


opts = info (fileInput <**> helper) fullDesc

runCli :: IO Input
runCli = execParser opts
