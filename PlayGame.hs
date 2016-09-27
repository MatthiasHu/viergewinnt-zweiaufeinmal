module PlayGame where

import Viergewinnt
import Data.List

decide :: Color -> Game -> Result
decide color state  = let 
	next = moves state color
	possibs = map (decide (otherColor color)) next
	oracle = if elem (Win color) possibs then Win color else
		if elem Draw possibs then Draw else Win (otherColor color)
	in case check state of 
		Just res -> res
		Nothing -> oracle
	
	
