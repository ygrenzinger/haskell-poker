module HandGenerator where

import Suit
import Rank
import Card

import Data.List

import Test.QuickCheck

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

generateHand :: Gen Hand
generateHand = vectorOf 5 generateCard

instance Arbitrary Card where 
    arbitrary = generateCard

removeElementsFrom :: Eq a => [a] -> [a] -> [a]
removeElementsFrom elements withoutElements = filter (\e -> not $ any (e ==) withoutElements) elements

generateCardWithout :: [Card] -> Gen Card
generateCardWithout withoutCards = elements $ removeElementsFrom allCards withoutCards

generateHandWithout :: [Card] -> Gen Hand
generateHandWithout withoutCards = vectorOf 5 (generateCardWithout withoutCards)

generateCardAtRankWithoutSuits :: Rank -> [Suit] -> [Card]
generateCardAtRankWithoutSuits rank withoutSuits = [Card rank suit | suit <- removeElementsFrom allSuits withoutSuits]

generateHandWithFourOfAKind :: Gen [Card]
generateHandWithFourOfAKind = do 
    rank <- elements allRanks
    let fourOfKind = generateCardAtRankWithoutSuits rank []
    c5 <- generateCardWithout fourOfKind
    return (c5 : fourOfKind)

someFunc :: IO ()
someFunc = putStrLn "someFunc"