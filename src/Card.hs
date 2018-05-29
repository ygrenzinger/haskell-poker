module Card where

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
        startRank <- enumFromTo Two Nine,
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

data HandCategory = None | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush deriving (Show, Eq, Ord, Enum, Bounded)

handCategory :: Hand -> HandCategory
handCategory hand 
    | isJust $ findStraightFlush hand = StraightFlush
    | isJust $ findFourOfAKind hand = FourOfAKind
    | isJust $ findFullHouse hand = FullHouse
    | isJust $ findFlush hand = Flush
    | isJust $ findStraight hand = Straight
    | isJust $ findThreeOfAKind hand = ThreeOfAKind
    | isJust $ findTwoPair hand = TwoPair
    | isJust $ findOnePair hand = OnePair
    | otherwise = None

-- computeHandValue :: Figure -> Hand -> (String, Int)
-- computeHandValue f@None hand = (show f, 0)
-- computeHandValue f@OnePair hand = (show f, 10^2)
-- computeHandValue f@TwoPair hand = (show f, 10^3)
-- computeHandValue f@ThreeOfAKind hand = (show f, 10^4)
-- computeHandValue f@Straight hand = (show f, 10^5)
-- computeHandValue f@Flush hand = (show f, 10^6)
-- computeHandValue f@FullHouse hand = (show f, 10^7)
-- computeHandValue f@FourOfAKind hand = (show f, 10^8)
-- computeHandValue f@StraightFlush hand = (show f, 10^9)

data Player = Player String Hand deriving (Eq)
instance Show Player where
    show (Player name hand) = "Player " ++ name ++ " with hand " ++ show hand

data Winner = Winner Player | DrawGame deriving (Eq)

instance Show Winner where
    show (Winner (Player name hand)) = name ++ " wins with " ++ (show $ handCategory hand)
    show DrawGame = "Draw game"

findWinner :: Player -> Player -> Winner
findWinner player1@(Player _ h1) player2@(Player _ h2) 
    | handCategory h1 > handCategory h2 = Winner player1
    | handCategory h1 < handCategory h2 = Winner player2
    | otherwise = DrawGame
