module Hand where

import Suit
import Rank

import Data.List
import Data.Maybe
import qualified Data.Text as T

data Card = Card Rank Suit deriving (Eq)

instance Show Card where
    show (Card r s) = show r ++ show s

parseCard :: T.Text -> Card
parseCard s = Card (parseRank (T.index s 0)) (parseSuit (T.index s 1))  

suit :: Card -> Suit
suit (Card _ s) = s 

rank :: Card -> Rank
rank (Card r _) = r

allCards :: [Card]
allCards = [Card r s | s <- allSuits, r <- allRanks] 

type Hand = [Card]

parseHand :: String -> Hand
parseHand s = map parseCard $ T.splitOn (T.pack " ") (T.pack s)

groupCardBy :: Eq a => (Card -> a) -> Hand -> [[a]]
groupCardBy f hand = groupBy (==) $ map f hand

findGroupOfRank :: Int -> Hand -> [Rank]
findGroupOfRank x hand = let 
    groupedByRank = filter (\innerlfindt -> (length innerlfindt) == x) (groupCardBy rank hand)
    in sort $ (map head groupedByRank)

findOnePair :: Hand -> Maybe Rank
findOnePair hand
    | onePair == [] = Nothing
    | otherwise = Just (head onePair)
    where onePair = findGroupOfRank 2 hand

findTwoPair :: Hand -> Maybe (Rank, Rank)
findTwoPair hand
    | length twoPair == 2 = Just (twoPair!!0, twoPair!!1)
    | otherwise = Nothing
    where twoPair = findGroupOfRank 2 hand

findXOfAKind :: Int -> Hand -> Maybe Rank
findXOfAKind x hand
    | xOfTheSameRank == [] = Nothing
    | otherwise = Just (head xOfTheSameRank)
    where xOfTheSameRank = findGroupOfRank x hand

findThreeOfAKind = findXOfAKind 3
findFourOfAKind = findXOfAKind 4

findFullHouse :: Hand -> Maybe (Rank, Rank)
findFullHouse hand = do
    threeRank <- findThreeOfAKind hand
    twoRank <- findOnePair hand
    return (threeRank, twoRank)

allPossibleStraights = 
    [ straight | 
        startRank <- enumFromTo Two Ten,
        let endRank = toEnum ((fromEnum startRank) + 4),
        let straight = enumFromTo startRank endRank
    ]

findStraight :: Hand -> Maybe Rank
findStraight hand = let 
    sortedRank = sort (map rank hand)
    findStraight = any (sortedRank ==) allPossibleStraights
    in if findStraight then Just (head sortedRank) else Nothing

findFlush :: Hand -> Maybe Suit
findFlush hand
    | flush == [] = Nothing
    | otherwise = Just ((head . head) flush)
    where flush = filter (\innerlfindt -> (length innerlfindt) == 5) (groupCardBy suit hand)

findStraightFlush :: Hand -> Maybe (Suit, Rank)
findStraightFlush hand = do
    flushSuit <- findFlush hand
    straightRank <- findStraight hand
    return (flushSuit, straightRank)

data HandCategory = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush deriving (Show, Eq, Ord, Enum, Bounded)

handCategory :: Hand -> HandCategory
handCategory hand 
    | isJust $ findStraightFlush hand = StraightFlush
    | isJust $ findFourOfAKind hand = FourOfAKind
    | isJust $ findFullHouse hand = FullHouse
    | isJust $ findFlush hand = Flush
    | isJust $ findStraight hand = Straight
    | isJust $ findThreeOfAKind hand = ThreeOfAKind
    | isJust $ findTwoPair hand = TwoPairs
    | isJust $ findOnePair hand = OnePair
    | otherwise = HighCard

allCategories :: [HandCategory]
allCategories = [minBound..]

data Player = Player String Hand deriving (Eq)
instance Show Player where
    show (Player name hand) = "Player " ++ name ++ " with hand " ++ show hand

data Winner = Winner Player | DrawGame deriving (Eq)
instance Show Winner where
    show (Winner (Player name hand)) = name ++ " wins with " ++ (show $ handCategory hand) ++ " " ++ (show $ hand)
    show DrawGame = "Draw game"

findWinner :: Player -> Player -> Winner
findWinner player1@(Player _ h1) player2@(Player _ h2)
    | handCategory1 > handCategory2 = Winner player1
    | handCategory1 < handCategory2 = Winner player2
    | otherwise = findWinnerWhenSameCategory handCategory1 player1 player2
    where handCategory1 = handCategory h1
          handCategory2 = handCategory h2

findWinnerWhenSameCategory :: HandCategory -> Player -> Player -> Winner
findWinnerWhenSameCategory HighCard p1 p2 = compareHigherCard p1 p2
findWinnerWhenSameCategory OnePair p1 p2 = comparePair p1 p2
findWinnerWhenSameCategory TwoPairs p1 p2 = comparePair p1 p2
findWinnerWhenSameCategory ThreeOfAKind p1 p2 = compareThreeOfAKind p1 p2
findWinnerWhenSameCategory Straight p1 p2 = compareStraight p1 p2
findWinnerWhenSameCategory Flush _ _ = DrawGame
findWinnerWhenSameCategory FullHouse p1 p2 = compareFullHouse p1 p2
findWinnerWhenSameCategory FourOfAKind p1 p2 = compareFourOfAKind p1 p2
findWinnerWhenSameCategory StraightFlush p1 p2 = compareStraight p1 p2

higherCard :: [Card] -> Rank
higherCard = maximum . (map rank)

compareHigherCard :: Player -> Player -> Winner
compareHigherCard player1@(Player _ h1) player2@(Player _ h2)
    | higherCard h1 > higherCard h2 = Winner player1
    | higherCard h1 < higherCard h2 = Winner player2
    | otherwise = DrawGame

compareStraight :: Player -> Player -> Winner
compareStraight player1@(Player _ h1) player2@(Player _ h2)
    | higherCard h1 > higherCard h2 = Winner player1
    | higherCard h1 < higherCard h2 = Winner player2
    | otherwise = DrawGame

compareFullHouse :: Player -> Player -> Winner
compareFullHouse player1@(Player _ h1) player2@(Player _ h2)
    | three h1 > three h2 = Winner player1
    | three h2 > three h1 = Winner player2
    | two h1 > two h2 = Winner player1
    | two h2 > two h1 = Winner player2
    | otherwise = DrawGame
    where 
        three = head . (findGroupOfRank 3)
        two = head . (findGroupOfRank 2)

compareXOfAKind :: Int -> Player -> Player -> Winner
compareXOfAKind x player1@(Player _ h1) player2@(Player _ h2)
    | xOfAKind h1 > xOfAKind h2 = Winner player1
    | xOfAKind h2 > xOfAKind h1 = Winner player2
    | otherCard h1 > otherCard h2 = Winner player1
    | otherCard h2 > otherCard h1 = Winner player2
    | otherwise = DrawGame
    where 
        xOfAKind = sort . (findGroupOfRank x)
        otherCard = sort . (findGroupOfRank 1)

compareFourOfAKind :: Player -> Player -> Winner
compareFourOfAKind = compareXOfAKind 4

compareThreeOfAKind :: Player -> Player -> Winner
compareThreeOfAKind = compareXOfAKind 3

comparePair :: Player -> Player -> Winner
comparePair = compareXOfAKind 2
