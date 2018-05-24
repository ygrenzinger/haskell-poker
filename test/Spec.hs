import HandGenerator

import Test.QuickCheck
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Suit
import Rank
import Card
import HandGenerator

main :: IO ()
main = hspec $ do
    describe "Poker Special Hand" $ do
        it "should have four of a kind" $ do
            let hand = parseHand "A♣ A♥ A♦ A♠ T♥"
            isFourOfAKind hand `shouldBe` Just Ace
        it "should have three of a kind" $ do
            let hand = parseHand "3♣ 3♥ 3♦ 4♠ T♥"
            isThreeOfAKind hand `shouldBe` Just Three
        it "should have two of a kind" $ do
            let hand = parseHand "J♣ J♥ Q♦ K♠ T♥"
            isTwoOfAKind hand `shouldBe` Just Jack
        it "should have full house" $ do
            let hand = parseHand "J♣ J♥ Q♦ Q♠ Q♥"
            isFullHouse hand `shouldBe` Just (Queen, Jack)
        it "should have straight" $ do
            let hand = parseHand "4♣ 5♥ 6♦ 7♠ 8♥"
            isStraight hand `shouldBe` Just Four
