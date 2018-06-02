
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

allEqual :: [Suit] -> Bool 
allEqual suits = and $ map (== head suits) (tail suits)

generateXDifferentSuits :: Int -> Gen [Suit]
generateXDifferentSuits x = suchThat (vectorOf x generateSuit) (not . allEqual)

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
generateXOfAKind x rank = do
    suits <- generateXDifferentSuits x
    return (map (Card rank) suits)

generateHighCard :: Gen [Card]
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
    c1 <- generateCardAtRank r2
    r3 <- generateRankWithout [r1, r2]
    c2 <- generateCardAtRank r3
    return ([c1, c2] ++ threeOfAKind)

generateStraight :: Gen [Card]
generateStraight = do
    suits <- generateXDifferentSuits 5
    ranks <- elements allPossibleStraights
    return (map (\(r, s) -> Card r s) (zip ranks suits))
    
generatePossibleFlush :: Gen [Card]
generatePossibleFlush = do
    suit <- elements allSuits
    cards <- vectorOf 5 $ generateCardAtSuit suit
    return cards
    
generateFlush :: Gen [Card]
generateFlush = suchThat generatePossibleFlush (\hand -> not $ any ((sort (map rank hand)) ==) allPossibleStraights)

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

generateSuperiorHandForFirstPlayer :: Gen (HandCategory, Player, HandCategory, Player, Winner)
generateSuperiorHandForFirstPlayer = do
    player1Category <- elements (enumFrom (toEnum 1))
    let player2Category = pred player1Category
    hand1 <- randomHandForCategory player1Category
    hand2 <- randomHandForCategory player2Category
    let player1 = Player "player1" hand1
    let player2 = Player "player2" hand2
    let gameResult = (findWinner player1 player2)
    return (player1Category, player1, player2Category, player2, gameResult)

prop_shouldGiveCorrectWinner :: Property
prop_shouldGiveCorrectWinner =
    forAll generateSuperiorHandForFirstPlayer 
    (\(player1Category, player1, player2Category, player2, gameResult) -> gameResult == (Winner player1))

main :: IO ()
main = hspec $ do
    describe "Using Property Based Testing" $ do
        it "should give winner the player with the highest poker's hand" $ do
            quickCheckWith Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 6000} prop_shouldGiveCorrectWinner

    describe "Poker Special Hand unit tests" $ do
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
            
        it "Comparing two Flush" $ do
            let player1 = (Player "Player1" (parseHand "9♣ 7♣ J♣ 2♣ K♣"))
            let player2 = (Player "Player2" (parseHand "6♥ 9♥ T♥ 4♥ 3♥"))
            findWinner player1 player2 `shouldBe` DrawGame
            
        it "Comparing two Straight" $ do
            let player1 = (Player "Player1" (parseHand "9♦ T♣ J♣ Q♣ K♦"))
            let player2 = (Player "Player2" (parseHand "8♦ 9♥ T♦ J♥ Q♥"))
            findWinner player1 player2 `shouldBe` Winner player1
            
        it "Comparing two Straight Flush" $ do
            let player1 = (Player "Player1" (parseHand "9♣ T♣ J♣ Q♣ K♣"))
            let player2 = (Player "Player2" (parseHand "8♥ 9♥ T♥ J♥ Q♥"))
            findWinner player1 player2 `shouldBe` Winner player1

        it "Comparing two FullHouse" $ do
            let player1 = (Player "Player1" (parseHand "A♣ A♥ Q♦ Q♠ Q♥"))
            let player2 = (Player "Player2" (parseHand "J♣ J♥ Q♦ Q♠ Q♥"))
            findWinner player1 player2 `shouldBe` Winner player1
            let player2 = (Player "Player2" (parseHand "J♣ J♥ K♦ K♠ K♥"))
            findWinner player1 player2 `shouldBe` Winner player2

        it "Comparing to High Card" $ do
            let player1 = (Player "Player1" (parseHand "3♣ 5♦ 6♥ A♥ J♥"))
            let player2 = (Player "Player2" (parseHand "5♠ 9♣ 5♥ J♠ K♦"))
            findWinner player1 player2 `shouldBe` Winner player1

        it "Comparing to Four of a Kind" $ do
            let player1 = (Player "Player1" (parseHand "3♥ K♦ K♦ K♣ K♠"))
            let player2 = (Player "Player2" (parseHand "A♦ T♦ T♥ T♥ T♦"))
            findWinner player1 player2 `shouldBe` Winner player1
            let player2 = (Player "Player2" (parseHand "A♦ K♦ K♦ K♣ K♠"))
            findWinner player1 player2 `shouldBe` Winner player2

        it "Comparing to Three of a Kind" $ do
            let player1 = (Player "Player1" (parseHand "3♥ 7♦ K♦ K♣ K♠"))
            let player2 = (Player "Player2" (parseHand "3♦ 8♦ T♥ T♥ T♦"))
            findWinner player1 player2 `shouldBe` Winner player1
            let player2 = (Player "Player2" (parseHand "3♦ 8♦ K♦ K♣ K♠"))
            findWinner player1 player2 `shouldBe` Winner player2

        it "Comparing to One Pair" $ do
            let player1 = (Player "Player1" (parseHand "3♥ 7♦ 4♦ K♣ K♠"))
            let player2 = (Player "Player2" (parseHand "3♦ 8♦ 6♥ T♥ T♦"))
            findWinner player1 player2 `shouldBe` Winner player1
            let player2 = (Player "Player2" (parseHand "3♦ 8♦ 6♦ K♣ K♠"))
            findWinner player1 player2 `shouldBe` Winner player2

        it "Comparing to Two Pair" $ do
            let player1 = (Player "Player1" (parseHand "3♦ Q♥ Q♦ K♣ K♠"))
            let player2 = (Player "Player2" (parseHand "4♦ 8♣ 8♠ K♥ K♦"))
            findWinner player1 player2 `shouldBe` Winner player1
            let player2 = (Player "Player2" (parseHand "4♦ Q♣ Q♠ K♥ K♦"))
            findWinner player1 player2 `shouldBe` Winner player2
