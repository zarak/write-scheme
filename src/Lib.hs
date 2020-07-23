module Lib where

-- |Exercise 1.
-- 
-- @
-- Change the program so it reads *two* arguments from the command line, and prints out a message using both of them
-- @
readTwoArgs :: [String] -> String
readTwoArgs [] = "" 
readTwoArgs [x] = x
readTwoArgs (x : y : xs) = x ++ " " ++ y
