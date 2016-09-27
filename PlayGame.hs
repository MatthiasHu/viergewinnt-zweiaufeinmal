module PlayGame where

import Viergewinnt
import Data.List
import Data.Ord (comparing)
import Data.Maybe (isJust)


decide :: Color -> Game -> (Int, Result, Game)
decide c g = (t, r, g')
  where
    l = decide' c g
    (Just r, g') = head . filter (isJust . fst) $ l
    t = length $ takeWhile (not . isJust . fst) l

decide' :: Color -> Game -> [(Maybe Result, Game)]
decide' c g =
  repeatFirstMatch (isJust . fst) $ (check g, g) : future
  where
    c' = otherColor c
    future =
        map (bestFor c)
      . transpose
      . map (\m -> [ (fst r, m) | r<-decide' c' m ])
      $ moves g c

bestFor :: Color -> [(Maybe Result, Game)] -> (Maybe Result, Game)
bestFor c [] = error "searching empty list of options"
bestFor c [r] = r
bestFor c (r1:r2:rs) =
  if fst r1 == Just (Win c) then r1
  else bestFor c $ maximumBy (comparing $ valueFor c . fst) [r1, r2] : rs
  where
    valueFor :: Color -> Maybe Result -> Int
    valueFor Blue Nothing = 0
    valueFor Blue (Just Draw) = 0
    valueFor Blue (Just (Win Blue)) = 1
    valueFor Blue (Just (Win Red)) = -1
    valueFor Red r = - valueFor Blue r

repeatFirstMatch :: (a -> Bool) -> [a] -> [a]
repeatFirstMatch t (a:as) =
  if t a then repeat a
  else a : repeatFirstMatch t as
