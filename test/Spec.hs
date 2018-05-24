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
            findFourOfAKind hand `shouldBe` Just Ace
            findFigure hand `shouldBe` Just FourOfAKind
        it "should have three of a kind" $ do
            let hand = parseHand "3♣ 3♥ 3♦ 4♠ T♥"
            findThreeOfAKind hand `shouldBe` Just Three
            findFigure hand `shouldBe` Just ThreeOfAKind
        it "should have one pair" $ do
            let hand = parseHand "J♣ J♥ Q♦ K♠ T♥"
            findOnePair hand `shouldBe` Just Jack
            findFigure hand `shouldBe` Just OnePair
        it "should have two pair" $ do
            let hand = parseHand "J♣ J♥ Q♦ K♠ K♥"
            findTwoPair hand `shouldBe` Just (Jack, King)
            findFigure hand `shouldBe` Just TwoPair
        it "should have full house" $ do
            let hand = parseHand "J♣ J♥ Q♦ Q♠ Q♥"
            findFullHouse hand `shouldBe` Just (Queen, Jack)
            findFigure hand `shouldBe` Just FullHouse
        it "should have straight" $ do
            let hand = parseHand "4♣ 5♥ 6♦ 7♠ 8♥"
            findStraight hand `shouldBe` Just Four
            findFigure hand `shouldBe` Just Straight
        it "should have flush" $ do
            let hand = parseHand "T♥ Q♥ 6♥ 2♥ 8♥"
            findFlush hand `shouldBe` Just Heart
            findFigure hand `shouldBe` Just Flush
        it "should have straight flush" $ do
            let hand = parseHand "4♥ 5♥ 6♥ 7♥ 8♥"
            findStraightFlush hand `shouldBe` Just (Heart, Four)
            findFigure hand `shouldBe` Just StraightFlush
