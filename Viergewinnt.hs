module Viergewinnt 
  ( Game
  , Color(..)
  , moves
  , winner
  ) where


data Color = Red | Blue

moves :: Game -> [Game]

winner :: Game -> Maybe Color
