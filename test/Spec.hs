import HandGenerator

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Test.QuickCheck


main :: IO ()
main = hspec $ do
    describe "Fake test" $ do
        it "should simulate test" $ do
            False `shouldBe` True
