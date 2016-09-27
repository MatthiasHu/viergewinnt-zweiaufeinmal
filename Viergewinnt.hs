module Viergewinnt 
  ( Game
  , Color(..)
  , Result(..)
  , otherColor
  , moves
  , check
  , startPosition
  ) where

import Data.Array


data Color = Red | Blue deriving (Eq)

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

data Game = Game
  { heights :: Array Int Int
  , slots   :: Array (Int, Int) (Maybe Color)
  }

startPosition :: Int -> Int -> Game
startPosition width height = undefined --Game
--  (array (0, width-1) [ 0 | x <- [0..width-1] ])
--  (array ((0, 0), (width-1, height-1))
--    [ Nothing | x <- [0..width-1], y <- [0..height-1] ] )

valid :: Game -> Bool
valid g =
  let (l, r) = bounds (heights g)
      ((l', b), (r', t)) = bounds (slots g)
  in l==l' && r==r' -- TODO: more invariants

data Result = Win Color | Draw deriving (Eq)


moves :: Game -> Color -> [Game]
moves = undefined

check :: Game -> Maybe Result
check = undefined
