import Exercises
import Test.Hspec
import Test.QuickCheck

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
