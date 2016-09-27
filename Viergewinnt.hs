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
import Data.Maybe


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
check state = let 
	((left,lower),(right,upper)) = bounds (slots state)
	verts = [[(i,j+k) | k<-[0..3]] | j<-[lower..upper-4],i<- [left..right]]
	hors = [[(i+k,j)| k<-[0..3]] | i<- [left..right-4],j<-[lower..upper]]
	diags = [[(i+k,j+k)|k<-[0..3]] | i<-[left..right-4],j<-[lower..upper-4]]
	antis = [[(i+k,j-k)|k<-[0..3]] | i<-[left..right-4],j<-[lower+4..upper]]
	fours = verts ++hors ++ diags ++ antis
	checkfour on four = if all (== head x) x then head x else Nothing where 
		x = map (on !) four
	checks = [fromJust t | t <- map (checkfour (slots state)) fours, t/=Nothing]
	full = all (==upper) $ elems $ heights state
	in 
	if null checks then 
		if full then Just Draw else Nothing 
	else Just (Win $ head checks)
