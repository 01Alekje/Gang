
{-
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}


module Simplify where

import Poly ( evalPoly, fromList, toList, Poly )
import Test.QuickCheck
import Data.Char


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving Eq -- JA

--------------------------------------------------------------------------------
-- * A1
-- Define a data type 'Expr' which represents three kinds of expression:
-- binary operators (use 'BinOp' as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should *not* use 'String' or 'Char' anywhere, since this is
-- not needed.

data Expr = Num   Int
          | Op BinOp Expr Expr
          | Pow   Int
          deriving (Eq)

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Num n) = True
prop_Expr (Pow n) = n >= 0
prop_Expr (Op binOp a b) = prop_Expr a && prop_Expr b

--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the
-- lecture). You can use Haskell notation for powers: x^2. You should show x^1
-- as just x.

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr expr = case expr of
  Num n -> show n
  Pow 1 -> "x"
  Pow n -> "x^" ++ show n
  Op binop a b -> showFactor (Op binop a b)
    where
      showFactor (Op binOp a b) = "(" ++ showExpr a ++ showbinop binOp ++ showExpr b ++ ")"


showbinop MulOp = "*"
showbinop AddOp = "+"
-------------------------------------------------------------------------------
-- * A4
-- Make 'Expr' an instance of 'Arbitrary'.
-- Now you can check the data type invariant that you defined in A2 using
-- QuickCheck.

instance Arbitrary Expr
  where arbitrary = sized genExpr

-- * Random generator with control over the size
genExpr :: Int -> Gen Expr
genExpr diff = frequency [ (diff `div` 2, genPow diff), (3, genNum), (diff, genOp diff)]

genNum = do
    n <- choose (1, 20)
    return (Num n)

genPow diff = do
    x <- choose (1, diff)
    return (Pow x)

genOp :: Int -> Gen Expr
genOp diff = let n = diff - (diff `div` 2) in do
  op <- elements [Op AddOp, Op MulOp]
  x  <- genExpr n
  y  <- genExpr n
  return (op x y)


--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.
eval :: Int -> Expr -> Int
eval _ (Num n)        = n
eval x (Pow n)        = x^n
eval x (Op AddOp a b) = eval x a + eval x b
eval x (Op MulOp a b) = eval x a * eval x b


--------------------------------------------------------------------------------
-- * A6
-- Define @exprToPoly@ that converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way.

ex1 :: Expr
ex1 = Op AddOp (Pow 2) (Pow 2) -- ska bli -> [1, 0, 1] -> x^2 + 1

ex2 :: Expr
ex2 = Op AddOp (Pow 2) (Op AddOp (Num 3) (Pow 4))

ex4 :: Expr
ex4 = Op MulOp (Op MulOp (Num 14) (Num 11)) (Num 14)

exprToPoly :: Expr -> Poly
exprToPoly (Num h) = fromList [h]
exprToPoly (Pow n) = fromList (1 : exprToPloy n)
exprToPoly (Op AddOp a b) = exprToPoly a + exprToPoly b
exprToPoly (Op MulOp a b) = exprToPoly a * exprToPoly b

exprToPloy :: Int -> [Int]
exprToPloy 0 = []
exprToPloy n = 0 : exprToPloy (n-1)



-- Define (and check) @prop_exprToPoly@, which checks that evaluating the
-- polynomial you get from @exprToPoly@ gives the same answer as evaluating
-- the expression.
prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e x = evalPoly x (exprToPoly e) == eval x e

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

--[1, 0, 1] -> x^2 + 1

polyToExpr :: Poly -> Expr
polyToExpr p = listToExpr (toList p)

listToExpr :: [Int] -> Expr
listToExpr []      = Num 0
listToExpr (0:xs)  = listToExpr xs
listToExpr (x:[])  = Num x
listToExpr (x:xs)
  | all (==0) xs = Pow (length xs)
  | otherwise = unique(Op AddOp (unique (Op MulOp (Num x) (Pow (length xs)))) (listToExpr xs))
                        where
                          unique :: Expr -> Expr
                          unique (Op MulOp (Num 1) (Pow n)) = (Pow n)
                          unique (Op MulOp (Pow n) (Num 1)) = (Pow n)
                          unique (Op MulOp a b) = Op MulOp a b
                          unique (Op AddOp (Num 0) (Pow n)) = (Pow n)
                          unique (Op AddOp (Pow n) (Num 0)) = (Pow n)
                          unique (Op AddOp a b) = Op AddOp a b


-- Write (and check) a quickCheck property for this function similar to
-- question 6.

prop_polyToExpr :: Poly -> Int -> Bool
prop_polyToExpr p x = eval x (polyToExpr p) == evalPoly x p

--------------------------------------------------------------------------------
-- * A8
-- Write a function @simplify@ which simplifies an expression by converting it
-- to a polynomial and back again.

simplify :: Expr -> Expr
simplify e = polyToExpr(exprToPoly e)


--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property that checks that a simplified expression does not
-- contain any "junk", where junk is defined to be multiplication by one or
-- zero, addition of zero, addition or multiplication of numbers, or x to the
-- power of zero. (You may need to fix A7)

prop_noJunk :: Expr -> Bool
prop_noJunk (Num n)        = True
prop_noJunk (Pow n)        = True
prop_noJunk (Op binOp a b) = check (simplify (Op binOp a b))

check :: Expr -> Bool
check (Op binOp (Num 0) _) = False
check (Op binOp _ (Num 0)) = False
check (Op MulOp _ (Num 1)) = False
check (Op MulOp (Num 1) _) = False
check (Op binOp b       a) = prop_noJunk (simplify a) && prop_noJunk (simplify b)
check _                    = True


--------------------------------------------------------------------------------
-- * A10
-- Write two IO functions that read respectively write the difficulty, which is
-- modelled as a natural number. Use the 'diffFile' as file path. Note that the
-- difficulty should never be below zero.

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do
  dstring <- readFile diffFile
  return (read dstring)
  

writeDifficulty :: Difficulty -> IO ()
writeDifficulty diff = do
  writeFile "difficulty.txt" (show diff)
  return ()


--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random
-- value for @x@, show the simplified expression and ask the user to solve it.
-- If the guess is as expected, give a nice feedback message and increase the
-- difficulty by one. If the guess was wrong, again give feedback and decrease
-- the difficulty by one. Then play again.

play :: IO ()
play = do
  randomx <- generate genX
  q <- readDifficulty
  randomExpr <- generate (genExpr q)

  putStrLn "Welcome to the polynomial calculation game!\n"
  putStrLn ("Simplify the following expression with x = " ++ show randomx ++ "\n")
  putStrLn (showExpr (simplify randomExpr) ++ "\n")

  answer <- readLn
  if eval randomx randomExpr == answer 
  then do
    putStr "Well Done!"

    -- n <- readLn
    -- writeFile difficulty.txt n+1
  else do
    putStrLn ("No, it should have been " ++ show (eval randomx randomExpr))
    -- y <- readDifficulty
    -- changeDifficulty' (writeDifficulty (x-1) )



{-
n <- readLn
-- Nu är n en Int
writeFile difficulty.txt n+1

dogg = y <- readLn


getDouble = dogg + 1
-}

cha :: IO Int
cha = do
  n <- readDifficulty
  -- writeFile diffFile (show n)
  -- writeDifficulty (n + 1)
  return (n)

-- changeDifficulty :: IO ()
-- changeDifficulty = do 
--   n <- readFile "difficulty.txt"
--   writeFile "difficulty.txt" (show ((digitToInt n) + 1))
  -- return ( writeFile writeDifficulty (n + 1))



-- changeDifficulty :: IO () -> IO a 
-- changeDifficulty = _

-- realChangeDiff :: Bool -> IO Read 
-- realChangeDiff True = do
--   diff <- readDifficulty
--   return (diff + 1)
-- realChangeDiff False = do
--   diff <- readDifficulty
--   return (diff + 1)

-- shit :: Difficulty
-- shit

-- readDifficulty >>= \line -> pure (line ++ "!")

-- realestChangeDiff :: IO Read 
-- realestChangeDiff readDifficulty >>= \line -> pure (line ++ "!")


-- changeDifficulty :: Bool ->  IO 
-- changeDifficulty = do
--   diff <- readDifficulty
  
-- changeDifficulty True = writeDifficulty (readDifficulty + 1)
-- changeDifficulty False
--   | readDifficulty == 1 = writeDifficulty 1
--   | otherwise = writeDifficulty (readDifficulty (read (- 1)))


genX :: Gen Int
genX = do choose (0, 5)

--------------------------------------------------------------------------------
