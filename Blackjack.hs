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

--allCard = allSuits ++  allRanks

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
playBank :: Deck -> Hand    
playBank deck = playBank' $ draw deck ([])
    where
        playBank' (d, h)
            | value h < 17 = playBank' $ draw d h
            | otherwise = h


-- B4
-- Given a list length [double] == 52, shuffle the items in deck
---shuffle :: [Double] -> Deck -> Deck
--shuffle randList n = [0..1]
    -- where n length = 52 
--{
    
--}
testRandList = [0.2, 0.52, 0.87, 0.1, 0.75, 0.34]

testDecker = [Card (Numeric 7) Spades
              ,Card (Numeric 4) Diamonds
              ,Card Jack Hearts
              ,Card (Numeric 9) Hearts
              ,Card (Numeric 6) Spades
              ,Card Queen Hearts
              ,Card (Numeric 3) Clubs
              ,Card (Numeric 2) Hearts
              ,Card (Numeric 5) Hearts
              ,Card (Numeric 4) Hearts]

realShuffle :: [Double] -> Deck -> Deck
realShuffle _ []    = []
realShuffle [] deck = deck
realShuffle randList (x:xs) = realShuffle' randList deck
    where
        realShuffle' (x:xs) (y:ys)
            | (value y > 7) && (x > 0.5) = realShuffle xs (ys:y)
            | otherwise = realShuffle ys --[z:(y:(ys))] -- ([y:z:ys])

{--
realestShuffle :: [Double] -> Deck -> Deck
realestShuffle _ []        = []
realestShuffle [] deck     = deck
realestShuffle random deck = shuffle'' random deck
    | 

--}