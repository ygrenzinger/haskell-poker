
import Suit
import Rank
import Hand

import Data.List

import Test.QuickCheck
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

generateRank :: Gen Rank
generateRank = elements allRanks

instance Arbitrary Rank where
    arbitrary = generateRank

generateSuit :: Gen Suit
generateSuit = elements allSuits

instance Arbitrary Suit where
    arbitrary = generateSuit

generateCard :: Gen Card
generateCard = elements allCards

instance Arbitrary Card where 
    arbitrary = generateCard

generateHand :: Gen Hand
generateHand = vectorOf 5 generateCard

removeElementsFrom :: Eq a => [a] -> [a] -> [a]
removeElementsFrom elements withoutElements = filter (\e -> not $ any (e ==) withoutElements) elements

generateRankWithout :: [Rank] -> Gen Rank
generateRankWithout withoutRanks = elements $ removeElementsFrom allRanks withoutRanks

generateCardWithout :: [Card] -> Gen Card
generateCardWithout withoutCards = elements $ removeElementsFrom allCards withoutCards

generateCardAtRank :: Rank -> Gen Card
generateCardAtRank rank = do
    suit <- generateSuit
    return (Card rank suit) 

generateCardAtSuit :: Suit -> Gen Card
generateCardAtSuit suit = do 
    rank <- generateRank
    return (Card rank suit)

generateXOfAKind :: Int -> Rank -> Gen [Card]
generateXOfAKind x rank = vectorOf x $ generateCardAtRank rank

generateHighCard = suchThat generateHand (\h -> handCategory h == HighCard)

-- could use a recursion or something more complex
generateOnePair :: Gen [Card]
generateOnePair = do
    r1 <- generateRank
    t1 <- generateXOfAKind 2 r1
    r2 <- generateRankWithout [r1]
    c1 <- generateCardAtRank r2
    r3 <- generateRankWithout [r1, r2]
    c2 <- generateCardAtRank r3
    r4 <- generateRankWithout [r1, r2, r3]
    c3 <- generateCardAtRank r4
    return ([c1, c2, c3] ++ t1)

generateTwoPairs :: Gen [Card]
generateTwoPairs = do
    r1 <- generateRank
    t1 <- generateXOfAKind 2 r1
    r2 <- generateRankWithout [r1]
    t2 <- generateXOfAKind 2 r2
    r3 <- generateRankWithout [r1, r2]
    c1 <- generateCardAtRank r3
    return ([c1] ++ t1 ++ t2)

generateThreeOfAKind :: Gen [Card]
generateThreeOfAKind = do
    r1 <- generateRank
    threeOfAKind <- generateXOfAKind 3 r1
    r2 <- generateRankWithout [r1]
    c1 <- generateCardAtRank r1
    r3 <- generateRankWithout [r1, r2]
    c2 <- generateCardAtRank r3
    return ([c1, c2] ++ threeOfAKind)

generateStraight :: Gen [Card]
generateStraight = do
    suits <- vectorOf 5 $ generateSuit
    ranks <- elements allPossibleStraights
    return (map (\(r, s) -> Card r s) (zip ranks suits))
    
generateFlush :: Gen [Card]
generateFlush = do
    suit <- elements allSuits
    cards <- vectorOf 5 $ generateCardAtSuit suit
    return cards

generateFullHouse :: Gen [Card]
generateFullHouse = do
    r1 <- generateRank
    threeOfAKind <- generateXOfAKind 3 r1
    r2 <- generateRankWithout [r1]
    twoOfAKind <- generateXOfAKind 2 r2
    return (twoOfAKind ++ threeOfAKind)

generateFourOfAKind :: Gen [Card]
generateFourOfAKind = do
    r1 <- generateRank
    fourOfKind <- generateXOfAKind 4 r1
    r2 <- generateRankWithout [r1]
    card <- generateCardAtRank r2
    return (card : fourOfKind)

generateStraightFlush :: Gen [Card]
generateStraightFlush = do
    suit <- generateSuit
    ranks <- elements allPossibleStraights
    return (map (\r -> Card r suit) ranks)

randomHandForCategory :: HandCategory -> Gen [Card]
randomHandForCategory HighCard = generateHighCard
randomHandForCategory OnePair = generateOnePair
randomHandForCategory TwoPairs = generateTwoPairs
randomHandForCategory ThreeOfAKind = generateThreeOfAKind
randomHandForCategory Straight = generateStraight
randomHandForCategory Flush = generateFlush
randomHandForCategory FullHouse = generateFullHouse
randomHandForCategory FourOfAKind = generateFourOfAKind
randomHandForCategory StraightFlush = generateStraightFlush

generateSuperiorHandForFirstPlayer :: Gen (Player, Player)
generateSuperiorHandForFirstPlayer = do
    category <- elements (enumFrom (toEnum 1))
    hand1 <- randomHandForCategory category
    hand2 <- randomHandForCategory (pred category)
    return (Player "player1" hand1, Player "player2" hand2)

prop_shouldGiveCorrectWinner :: Property
prop_shouldGiveCorrectWinner =
    forAll generateSuperiorHandForFirstPlayer 
    (\(player1, player2) -> (findWinner player1 player2) == (Winner player1))

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
            handCategory hand `shouldBe` TwoPairs
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
    
        it "should give winner the player with the highest poker's hand" $ do
            prop_shouldGiveCorrectWinner
