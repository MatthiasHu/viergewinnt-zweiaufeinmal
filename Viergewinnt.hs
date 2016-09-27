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
import Data.List (intersperse)


data Color = Red | Blue
  deriving (Eq, Ord, Show)

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

data Game = Game
  { heights :: Array Int Int
  , slots   :: Array (Int, Int) (Maybe Color)
  }

startPosition :: Int -> Int -> Game
startPosition width height = Game
  (array (0, width-1) [ (x, 0) | x <- [0..width-1] ])
  (array ((0, 0), (width-1, height-1))
    [ ((x, y), Nothing) | x <- [0..width-1], y <- [0..height-1] ] )

instance Show Game where
  show g = unlines $ [ line (h-i) | i<-[0..h-1] ] ++ [replicate (2*w+1) '-']
    where
      s = slots g
      w = fst . snd . bounds $ s
      h = snd . snd . bounds $ s
      line y =
        '|' :
        intersperse ' ' [ colorChar (s ! (x, y)) | x <- [0..w-1] ]
        ++ "|"
      colorChar :: Maybe Color -> Char
      colorChar Nothing = ' '
      colorChar (Just Red) = 'X'
      colorChar (Just Blue) = 'O'


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
