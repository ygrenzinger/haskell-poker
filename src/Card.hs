module Card where

import Suit
import Rank

import Data.List

data Card = Card Rank Suit deriving (Eq)

instance Show Card where
    show (Card r s) = show r ++ show s

parseCard :: [Char] -> Card
parseCard s = Card (parseRank (s!!0)) (parseSuit (s!!1))  

suit :: Card -> Suit
suit (Card _ s) = s 

rank :: Card -> Rank
rank (Card r _) = r

allCards :: [Card]
allCards = [Card r s | s <- allSuits, r <- allRanks] 

type Hand = [Card]

--parseHand :: [Char] -> Hand
--parseHand s = map parseCard $ splitOn " " s

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
