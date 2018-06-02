module Main where

import Hand

main :: IO ()
main = do
    putStrLn "Enter the hand of player 1 (like 3♦ Q♥ Q♦ K♣ K♠) : "
    hand1 <- getLine
    putStrLn "Enter the hand of player 2 (like 9♣ T♣ J♣ Q♣ K♣) : "
    hand2 <- getLine
    let player1 = (Player "Player 1" (parseHand hand1))
    let player2 = (Player "Player 2" (parseHand hand2))
    let winner = findWinner player1 player2
    putStrLn (show winner)

