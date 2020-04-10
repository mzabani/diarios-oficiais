module Main where

import DiariosOficiais (start)
import qualified System.IO as IO

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    start
