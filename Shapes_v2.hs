{--

Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Oliver Ivarsson, Omar Ahmed, Alexander Kjellberg
Lab group   : Grupp 33
--}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck
-- import Text.Xhtml (Shape)
import Data.Bool (Bool)
import GHC.Base (undefined)

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

--data Shape = Shape [Row]

--rows :: Shape -> [Row]
--rows (Shape x) = x

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]

  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes]
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (x, y) = Shape $ replicate y (emptyShape' x)
  where
    emptyShape' x = replicate x Nothing

-- ** A2
shapeSize :: Shape -> (Int, Int)
shapeSize shape = (length (head (rows shape)), length (rows shape))

-- ** A3
-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount shape = blockCount' 0 $ concat (rows shape)
  where
    blockCount' count [] = count
    blockCount' count (x:xs)
      | x /= Nothing = blockCount' (count+1) xs
      | otherwise = blockCount' count xs

-- * The Shape invariant
-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column, and are rectangular.

prop_Shape :: Shape -> Bool
prop_Shape shape = (x > 0 && y > 0) && checkRowsSame (rows shape)
  where (x,y) = shapeSize shape

checkRowsSame :: [Row] -> Bool
checkRowsSame [] = True
checkRowsSame [[]] = True
checkRowsSame (x:y:[]) = length x == length y
checkRowsSame (x:y:xs) = (length x == length y) && checkRowsSame (y:xs)
checkRowsSame [_:_] = False

-- -- * Test data generators

-- -- ** A5
-- -- | A random generator for colours
genColour :: Gen Colour
genColour = elements [ Red, Grey, Blue, Yellow, Cyan, Green, Purple ]

instance Arbitrary Colour where
  arbitrary = genColour

-- -- ** A6
-- -- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
     arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape shape) = Shape (transpose (reverse shape))

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (r, d) (Shape shape) = shiftDown d (shiftRight r (Shape shape))

test = allShapes !! 1
test2 = allShapes !! 2

shiftDown :: Int -> Shape -> Shape
shiftDown 0 (Shape shape) = Shape shape
shiftDown x (Shape shape) = shiftDown (x-1) $ Shape (addEmptyRow (fst (shapeSize (Shape shape))) : shape)

addEmptyRow :: Int -> Row
addEmptyRow x = replicate x Nothing

shiftRight :: Int -> Shape -> Shape
shiftRight 0 (Shape shape) = Shape shape
shiftRight x (Shape shape) = shiftRight (x-1) (Shape (map addEmptyRight shape))
  where
    addEmptyRight :: Row -> Row
    addEmptyRight r1 = Nothing : r1


-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (r, d) (Shape shape) = shiftLeft r $ shiftUp d (Shape shape)

shiftUp :: Int -> Shape -> Shape
shiftUp 0 (Shape shape) = Shape shape
shiftUp x xs@(Shape shape) = Shape (shape ++ replicate x (addEmptyRow y))
  where y = (fst . shapeSize) xs

shiftLeft :: Int -> Shape -> Shape
shiftLeft 0 (Shape shape) = Shape shape
shiftLeft x (Shape shape) = shiftLeft  (x-1) $ Shape (map addEmptyLeft shape) 
  where
    addEmptyLeft :: Row -> Row
    addEmptyLeft r1 = r1 ++ [Nothing]

-- ** A10

-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (x, y) (Shape shape) = padShape (x-x2, y-y2) (Shape shape)
  where
    (x2,y2) = shapeSize (Shape shape)


-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 =  rowsOverlap $ zip (rows s1) (rows s2)
  where
    rowsOverlap :: [(Row,Row)] -> Bool
    rowsOverlap [] = False
    rowsOverlap (x:[]) = collisionCheck x
    rowsOverlap (x:xs) = collisionCheck x && rowsOverlap xs 

collisionCheck :: ([Square], [Square]) -> Bool
collisionCheck (_, []) = False
collisionCheck ([], _) = False
collisionCheck (a:as, b:bs)
  | a /= Nothing && b /= Nothing = True
  | otherwise = collisionCheck (as, bs)
  

-- B2
-- use zipWith twice to combine two shapes in the same way that zipWith
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (Shape shape1) (Shape shape2) = Shape (zipWith (zipRowWith f) shape1 shape2)

-- foo :: Row -> Row -> Row
-- shape1 :: [Row]
-- shape2 :: [Row]

zipRowWith :: (Square -> Square -> Square) -> Row -> Row -> Row
zipRowWith f r1 r2 = zipWith f r1 r2

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"