module Suit where 

data Suit = Club | Spade | Diamond | Heart deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
    show (Club) = "♣"
    show (Diamond) = "♦"
    show (Spade) = "♠"
    show (Heart) = "♥"

parseSuit :: Char -> Suit
parseSuit '♣' = Club
parseSuit '♦' = Diamond
parseSuit '♠' = Spade
parseSuit '♥' = Heart

allSuits :: [Suit]
allSuits = [minBound..]