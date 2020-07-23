module Main where

import Lib
import Exercises
import System.Environment


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
