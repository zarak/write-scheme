import Exercises
import Lib
import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.Parsec hiding (spaces)


readExpr' :: String -> Parser LispVal -> String
readExpr' input parser = case parse parser "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Exercise 1" $
        it "should concatenate the first two words" $
            readTwoArgs ["hello", "there", "sir"] `shouldBe` "hello there"
    describe "Exercies 2" $
        it "should add the first two numbers" $
            arithmetic ["1", "2", "4"] `shouldBe` "3"
    describe "Exercise 4" $
        it "should give the same result as the original parseNumber" $
            readExpr' "123" Lib.parseNumber `shouldBe` readExpr' "123" Exercises.parseNumber
