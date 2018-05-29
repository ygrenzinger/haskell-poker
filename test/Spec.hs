import HandGenerator

import Test.QuickCheck
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Suit
import Rank
import Card
import HandGenerator

randomGame :: Gen (Player, Player, Winner)
randomGame = do
    hand1 <- generateHand
    let player1 = Player "Player 1" hand1
    hand2 <- generateHandWithout hand1
    let player2 = Player "Player 2" hand2
    return (player1, player2, (findWinner player1 player2))

isVictoryCorrect :: Winner -> Player -> Player -> Bool
isVictoryCorrect (Winner player) player1@(Player _ h1) player2@(Player _ h2) = if player == player1 
        then handCategory h1 > handCategory h2
        else handCategory h1 < handCategory h2
isVictoryCorrect DrawGame (Player _ h1) (Player _ h2) = handCategory h1 == handCategory h2

prop_shouldGiveCorrectWinner :: Property
prop_shouldGiveCorrectWinner =
    forAll randomGame
    (\(player1, player2, winner) -> isVictoryCorrect winner player1 player2)

main :: IO ()
main = hspec $ do
    describe "Poker Special Hand" $ do
        it "should have four of a kind" $ do
            let hand = parseHand "A♣ A♥ A♦ A♠ T♥"
            findFourOfAKind hand `shouldBe` Just Ace
            handCategory hand `shouldBe` FourOfAKind
        it "should have three of a kind" $ do
            let hand = parseHand "3♣ 3♥ 3♦ 4♠ T♥"
            findThreeOfAKind hand `shouldBe` Just Three
            handCategory hand `shouldBe` ThreeOfAKind
        it "should have one pair" $ do
            let hand = parseHand "J♣ J♥ Q♦ K♠ T♥"
            findOnePair hand `shouldBe` Just Jack
            handCategory hand `shouldBe` OnePair
        it "should have two pair" $ do
            let hand = parseHand "J♣ J♥ Q♦ K♠ K♥"
            findTwoPair hand `shouldBe` Just (Jack, King)
            handCategory hand `shouldBe` TwoPair
        it "should have full house" $ do
            let hand = parseHand "J♣ J♥ Q♦ Q♠ Q♥"
            findFullHouse hand `shouldBe` Just (Queen, Jack)
            handCategory hand `shouldBe` FullHouse
        it "should have straight" $ do
            let hand = parseHand "4♣ 5♥ 6♦ 7♠ 8♥"
            findStraight hand `shouldBe` Just Four
            handCategory hand `shouldBe` Straight
        it "should have flush" $ do
            let hand = parseHand "T♥ Q♥ 6♥ 2♥ 8♥"
            findFlush hand `shouldBe` Just Heart
            handCategory hand `shouldBe` Flush
        it "should have straight flush" $ do
            let hand = parseHand "4♥ 5♥ 6♥ 7♥ 8♥"
            findStraightFlush hand `shouldBe` Just (Heart, Four)
            handCategory hand `shouldBe` StraightFlush
    
        it "should given winner between two hands" $ do
            prop_shouldGiveCorrectWinner
