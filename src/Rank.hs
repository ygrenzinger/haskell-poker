module Rank where 

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Bounded)

allRanks :: [Rank]
allRanks = [minBound..]

instance Show Rank where
    show (Two) = "2"
    show (Three) = "3"
    show (Four) = "4"
    show (Five) = "5"
    show (Six) = "6"
    show (Seven) = "7"
    show (Eight) = "8"
    show (Nine) = "9"
    show (Ten) = "T"
    show (Jack) = "J"
    show (Queen) = "Q"
    show (King) = "K"
    show (Ace) = "A"

parseRank :: Char -> Rank
parseRank '2' = Two
parseRank '3' = Three
parseRank '4' = Four
parseRank '5' = Five
parseRank '6' = Six
parseRank '7' = Seven
parseRank '8' = Eight
parseRank '9' = Nine
parseRank 'T' = Ten
parseRank 'J' = Jack
parseRank 'Q' = Queen
parseRank 'K' = King
parseRank 'A' = Ace
