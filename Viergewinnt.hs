module Viergewinnt 
  ( Game
  , Color(..)
  , Result(..)
  , otherColor
  , moves
  , check
  ) where


data Color = Red | Blue deriving (Eq)

otherColor :: Color -> Color

data Result = Win Color | Draw


moves :: Game -> Color -> [Game]

check :: Game -> Maybe Result
