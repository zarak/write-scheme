module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ readTwoArgs args)
