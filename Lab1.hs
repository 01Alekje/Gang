-- import MeasureTime.hs

{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT440, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Oliver Ivarsson, Omar Ahmed, Alexander Kjellberg
Lab group   : Grupp 33
-}

-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
power :: Integer -> Integer -> Integer
power n k 
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


-- Part A ----------------------------------------------------------------------

-- stepsPower k gives the number of multiplications executed by power n k
stepsPower :: Integer -> Integer
stepsPower k
  | k < 0 = error "power: negative argument"
  | otherwise = k


-- Part B ----------------------------------------------------------------------

power1 :: Integer -> Integer -> Integer
power1 n k
  | k == 0 = 1
  | otherwise = product (replicate (fromInteger k) n)


-- Part C ----------------------------------------------------------------------

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
  | even k = power2 (n * n) (div k 2)
  | odd k = n * power2 n (k-1)
  | k < 0 = error "power: negative argument"
  | otherwise = error "unknown problem"


-- Part D ----------------------------------------------------------------------

-- A function to compare power and power1 given the same values
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

-- A function to compare power and power2 given the same values
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

-- We want to test all guard conditions atleast once. In other words when k = 0, when k is even and when k is odd
test, test1, test2, test3 :: Bool
test  = test1 && test2 && test3

test1 = comparePower1 1 0 == comparePower2 1 0

test2 = comparePower2 2 3 == comparePower1 2 3

test3 = comparePower2 2 2 == comparePower1 2 2

-- A function to test all functions with costumized arguments given in ghci 
testAll :: Integer -> Integer -> Bool
testAll n k = comparePower1 n k && comparePower2 n k