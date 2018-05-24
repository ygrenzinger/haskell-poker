module Card where

import Suit
import Rank

import Data.List
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

groupedByRank :: Hand -> [[Rank]]
groupedByRank hand = groupBy (==) $ map rank hand

isXOfAKind :: Int -> Hand -> Maybe Rank
isXOfAKind x hand
    | xOfTheSameRank == [] = Nothing
    | otherwise = Just ((head . head) xOfTheSameRank)
    where xOfTheSameRank = filter (\innerlist -> (length innerlist) == x) (groupedByRank hand)

isFourOfAKind = isXOfAKind 4
isThreeOfAKind = isXOfAKind 3
isTwoOfAKind = isXOfAKind 2

isFullHouse :: Hand -> Maybe (Rank, Rank)
isFullHouse hand = do
    three <- isThreeOfAKind hand
    two <- isTwoOfAKind hand
    return (three, two)

allPossibleStraights = 
    [ straight | 
        startRank <- enumFromTo Two Nine,
        let endRank = toEnum ((fromEnum startRank) + 4),
        let straight = enumFromTo startRank endRank
    ]

isStraight :: Hand -> Maybe Rank
isStraight hand = let 
    sortedRank = sort (map rank hand)
    isStraight = any (sortedRank ==) allPossibleStraights
    in if isStraight then Just (head sortedRank) else Nothing