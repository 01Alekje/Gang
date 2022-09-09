{- |
Module      : Blackjack
License     : BSD
Stability   : experimental
Authors     : Oliver Ivarsson, Omar Ahmed, Alexander Kjellberg
Lab group   : Grupp 33
-}

module Blackjack where

import Cards
import RunGame
import Data.ByteString (last)

-- some hands for testing purposes
handPlayer1 = [Card (Numeric 2) Spades
              ,Card Ace Diamonds
              ,Card Ace Hearts] -- value = 4

handBank1 = [Card King Clubs
            ,Card Queen Hearts
            ,Card (Numeric 2) Clubs] -- value = 22

handPlayer2 = [Card (Numeric 7) Spades
              ,Card (Numeric 4) Diamonds
              ,Card (Numeric 9) Hearts] -- value = 20

handBank2 = [Card Jack Spades
            ,Card Jack Hearts
            ,Card King Clubs] -- value = 30

-- Task A1
-- Execute the expression size hand2 by hand, step by step:
-- size hand2
--     = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
--     = 1 + size (Card Jack Spades : [])
--     = 1 + (1 + ( 0 ))
--     = 2

-- Write this sequence of equations as a Haskell definition in the following manner:
-- sizeSteps :: [Int]
-- sizeSteps = [ size hand2
--             , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
--             , 1 + size (Card Jack Spades : [])
--             , 2
--             ]


-- Task A2 

display :: Hand -> String
display [] = ""
display (card:[]) = show (rank card) ++ " of " ++ show (suit card)
display (card:hand) = show (rank card) ++ " of " ++ show (suit card) ++ ", " ++ display hand

-- Task A3

-- Returns the value of a given rank
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

-- Returns the number of aces in a given hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (x:xs)
    | rank x == Ace = 1 + numberOfAces xs
    | otherwise = numberOfAces xs

-- Returns the value of given hand
valueCheck :: Hand -> Int
valueCheck [] = 0
valueCheck (card:hand) = valueRank (rank card) + valueCheck hand

-- Returns the value of given hand and lowers the value if it surpasses 21 and contains an ace
value :: Hand -> Int
value hand
    | valueCheck hand > 21 = valueCheck hand - (numberOfAces hand) * 10
    | otherwise = valueCheck hand

-- Task A4

-- Checks whether a hand has gone bust or not
gameOver :: Hand -> Bool 
gameOver hand
    | value hand > 21 = True
    | otherwise = False 

-- Checks which player won
winner :: Hand -> Hand -> Player
winner handPlayer handBank
    | gameOver handPlayer = Bank
    | gameOver handBank = Guest
    | value handPlayer > value handBank = Guest
    | value handBank > value handPlayer = Bank
    | value handBank == value handPlayer = Bank