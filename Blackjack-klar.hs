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
import Test.QuickCheck hiding (shuffle)

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
displayCard :: Card -> String
displayCard (Card (Numeric r) s) = show (r) ++ " of " ++ show (s) 
displayCard (Card r s) = show (r) ++ " of " ++ show (s)

display :: Hand -> String
display [] = ""
display (x:[]) = displayCard x
display (x:hand) = displayCard x ++ ", " ++ display hand


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
gameOver hand = (value hand) > 21

-- Checks which player won
winner :: Hand -> Hand -> Player
winner handPlayer handBank
    | gameOver handPlayer = Bank
    | gameOver handBank = Guest
    | value handPlayer > value handBank = Guest
    | value handBank > value handPlayer = Bank
    | value handBank == value handPlayer = Bank


-----------------------------------------------------------------------------------------------

-- Task B1
-- QuickCheck prop for fullDeck
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

allRanks :: [Rank]
allRanks = dressed ++ (reverse allNums)

dressed :: [Rank]
dressed = [Ace, King, Queen, Jack]

allNums :: [Rank]
allNums = [Numeric x | x <- numberlist]
  where
    numberlist = [2..10]

-- temporary test deck
testDeck = [Card (Numeric 2) Spades
              ,Card Queen Diamonds
              ,Card (Numeric 5) Hearts
              ,Card (Numeric 8) Clubs]


-- A function that returns a full deck of cards !Gör KORTARE!
fullDeck = (zip' allRanks [Spades]) 
            ++ (zip' allRanks [Hearts]) 
            ++ (zip' allRanks [Diamonds]) 
            ++ (zip' allRanks [Clubs])
-- | Combining two lists into a list of pairs:
zip' :: [Rank] -> [Suit] -> Deck
zip' (x:xs) (y:ys) = (Card x y) : zip' xs (y:ys)
zip' []        _  = []


-- B2
-- draws the first card in the deck and appends it to 'given' hand
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "The deck is empty!"
draw (x:xs) hand = (xs ,(x : hand))


-- B3
-- gives the bank its full hand
playBank :: Deck -> Hand    
playBank deck = playBank' $ draw deck ([])
    where
        playBank' (d, h)
            | value h < 17 = playBank' $ draw d h
            | otherwise = h


-- B4
-- Takes in a deck of cards and shuffles it
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (d:ds) deck = randomCard : shuffle ds deck'
    where
        randomNum = randomIndex d (length deck)
        (randomCard, deck') = realTakeIndex randomNum deck


-- returns a 'random' index in a list
randomIndex :: Double -> Int -> Int
randomIndex n len = floor $ n * fromIntegral len


-- takes an integer (index to separate from deck) and deck, and returns (<card on desired index>, <deck without card on desired index>)
realTakeIndex :: Int -> Deck -> (Card, Deck)
realTakeIndex n d = (d !! n, deleteIndex n d)


-- removes the item on index k from a list
deleteIndex :: Int -> [a] -> [a]
deleteIndex _ []     = []
deleteIndex k (x:xs)
   | k == 0    = xs
   | otherwise = x : deleteIndex (k-1) xs


-- B5
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck


prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = length deck == length (shuffle randomlist deck)

-- B6

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation