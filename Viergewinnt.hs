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
import Data.Maybe (fromJust)
import Data.List (intersperse)
import Data.Maybe (catMaybes)


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
  show g = unlines $ [ line (h-i) | i<-[0..h] ] ++ [replicate (2*w+1) '-']
    where
      s = slots g
      w = fst . snd . bounds $ s
      h = snd . snd . bounds $ s
      line y =
        '|' :
        intersperse ' ' [ colorChar (s ! (x, y)) | x <- [0..w] ]
        ++ "|"
      colorChar :: Maybe Color -> Char
      colorChar Nothing = ' '
      colorChar (Just Red) = 'X'
      colorChar (Just Blue) = 'O'


valid :: Game -> Bool
valid g =
  let (l, r) = bounds (heights g)
      ((l', b), (r', t)) = bounds (slots g)
  in l==0 && b==0 && l==l' && r==r' -- TODO: more invariants

data Result = Win Color | Draw
  deriving (Eq, Ord, Show)


dropPiece :: Color -> Int -> Game -> Game
dropPiece c x g =
  g { slots = newS, heights = newH }
  where
    oldS = slots g
    oldH = heights g
    hx = oldH ! x
    h = snd . snd . bounds $ oldS
    newH = if hx>h then error "dropping in full column"
           else oldH // [(x, hx+1)]
    newS = oldS // [((x, hx), Just c)]

legalDrop :: Int -> Game -> Bool
legalDrop x g = (heights g ! x) <= (snd . snd . bounds . slots $ g)

moves :: Game -> Color -> [Game]
moves g c = if null outcomes then [g]
            else outcomes
  where
    choices :: [[Int]] -- list of pairs
    choices = [ [x1, x2] | x1 <- [0..w], x2 <- [x1..w] ]
    w = fst . snd . bounds . slots $ g
    outcomes = catMaybes $ [ outcome xs g | xs <- choices ]
    outcome :: [Int] -> Game -> Maybe Game -- drop multiple pieces if possible
    outcome [] g' = Just g'
    outcome (x:xs) g' =
      if legalDrop x g' then outcome xs (dropPiece c x g')
      else Nothing


check :: Game -> Maybe Result
check state = let 
	((left,lower),(right,upper)) = bounds (slots state)
	verts = [[(i,j+k) | k<-[0..3]] | j<-[lower..upper-3],i<- [left..right]]
	hors = [[(i+k,j)| k<-[0..3]] | i<- [left..right-3],j<-[lower..upper]]
	diags = [[(i+k,j+k)|k<-[0..3]] | i<-[left..right-3],j<-[lower..upper-3]]
	antis = [[(i+k,j-k)|k<-[0..3]] | i<-[left..right-3],j<-[lower+3..upper]]
	fours = verts ++hors ++ diags ++ antis
	checkfour on four = if all (== head x) x then head x else Nothing where 
		x = map (on !) four
	checks = [fromJust t | t <- map (checkfour (slots state)) fours, t/=Nothing]
	full = all (==upper) $ elems $ heights state
	in 
	if null checks then 
		if full then Just Draw else Nothing 
	else Just (Win $ head checks)
