module Exercises where

import Text.ParserCombinators.Parsec hiding (spaces)

-- |Exercise 1
-- 
-- @
-- Change the program so it reads *two* arguments from the command line, and prints out a message using both of them
-- @
readTwoArgs :: [String] -> String
readTwoArgs = unwords . take 2

-- |Exercise 2
-- 
-- @
-- Change the program so it performs a simple arithmetic operation on the two arguments and prints out the result. 
-- @
arithmetic :: [String] -> String
arithmetic = show . sum . map read . take 2

