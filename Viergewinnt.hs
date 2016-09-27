module Viergewinnt 
  ( Game
  , Color(..)
  , Result(..)
  , otherColor
  , moves
  , check
  ) where

import Data.Array


data Color = Red | Blue deriving (Eq)

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

data Game = Game
  { heights :: Array Int Int
  , slots   :: Array (Int, Int) Color
  }

valid :: Game -> Bool
valid g =
  let (l, r) = bounds (heights g)
      ((l', b), (r', t)) = bounds (slots g)
  in l==l' && r==r' -- TODO: more invariants

data Result = Win Color | Draw


moves :: Game -> Color -> [Game]
moves = undefined

check :: Game -> Maybe Result
check = undefined
