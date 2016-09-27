module PlayGame where

import Viergewinnt
import Data.List

decide :: Color -> Game -> Result
decide color state  = let 
	next = moves state color
	possibs = map (decide (otherColor color)) next
	oracle = if exists (color==) possibs then color else
		if exists (Draw==) possibs then Draw else otherColor color
	in case check state of 
		Just res -> res
		Nothing -> oracle
	
	
