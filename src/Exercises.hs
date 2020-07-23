module Exercises where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Lib
import Data.Char

-- |__Exercise 1__
-- 
-- Change the program so it reads /two/ arguments from the command line, and prints out a message using both of them.
readTwoArgs :: [String] -> String
readTwoArgs = unwords . take 2

-- |__Exercise 2__
-- 
-- Change the program so it performs a simple arithmetic operation on the two arguments and prints out the result. 
arithmetic :: [String] -> String
arithmetic = show . sum . map read . take 2

-- |__Exercise 3__
--
-- @getLine@ is an IO action that reads a line from the console and returns it as a string. 
-- Change the program so it prompts for a name, reads the name, and then prints that instead of the command line value.
promptForName :: IO ()
promptForName = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

-- |__Exercise 4__
--
-- Rewrite @parseNumber@, without @liftM@, using
--
-- 1. do-notation
-- 2. explicit sequencing with the @>>=@ operator
parseNumber :: Parser LispVal
parseNumber = do
    d <- many1 digit
    return $ Number (read d :: Integer)

-- |__Exercise 4__
--
-- Equivalent version of `parseNumber` using @>>=@.
parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= return . Number . read


-- |__Exercise 5__
--
-- Our strings aren't quite R5RS compliant, because they don't support escaping of internal quotes within the string.  
-- Change 'parseString' so that @\"@ gives a literal quote character instead of terminating the string.  
-- You may want to replace @noneOf "\""@ with a new parser action that accepts /either/ a non-quote character /or/ a backslash followed by a quote mark.
parseEsc = undefined
