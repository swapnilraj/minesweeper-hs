{- | Module 'AI', tries to play the game using some heuristics.

    For a completely 'Hidden' 'Board', always pick the top-left 'Cell'.

    Uses two heuristics:

    * If the number of 'Hidden' cells around a 'Numbered' cell are equal then
    they must be a 'Mine'.

    * If the number of 'Flagged' cells around a 'Numbered' cell are equal then
    they must be an 'Empty' cell.
-}
module AI
  ( AIMove
  , solve
  ) where

import AIMine
  ( Board(..)
  , Cell(..)
  , Point(..)
  , (!?)
  , allHidden
  , assocs
  , clip
  , size
  , genNeighbours
  )

import Gameplay(Move(..))
import Control.Monad(forM_)

-- | Represents the 'Move' made by the 'AI'.
type AIMove = (Move, Point)
-- | Utility function to make an 'AIMove'.
--
-- Private to the module.
mkMove m l = (,) m l

-- | Co-ordinates for all the cells. 
--
-- Private to the module.
points size= [ [ (x, y) |  x <- [0..size-1] ] | y <- [0..size-1]]

-- | Returns 'True' if a cell is 'Numbered'.
--
-- Private to the module.
isNumbered (Numbered _) = True
isNumbered _ = False

-- | Reads a 'Board' and tries to suggest the next 'AIMove', uses all
-- heuristics that the module provides.
solve :: Board -> Maybe [AIMove]
solve b
  -- If all cells are closed guess an edge cell probability tells us that an
  -- edge cell is least likely to have a mine
  | allHidden b = Just $ [ mkMove Reveal (0, 0) ]
  | otherwise =
    let sz = size b
        onlyNumbered = filter (isNumbered . snd) (assocs b) in
        case (length onlyNumbered == 0) of
          True -> Nothing
          False -> Just $ flattenMybLst $ (\(p, val) -> decideMove b p sz val) <$> onlyNumbered

-- | Flattens a list of Maybe [a].
--
-- Private to the module.
flattenMybLst :: [ Maybe [a] ] -> [a]
flattenMybLst = foldl (\acc v -> acc ++ myb2Lst v) []
  where
    myb2Lst (Just n) = n
    myb2Lst Nothing = []

-- Decides the next 'Move' based on the two heuristics.
--
-- Private to the module.
decideMove :: Board -> Point -> Int -> Cell -> Maybe [AIMove]
decideMove b p sz (Numbered n)
  | n == count b Hidden (neighbours p sz) = Just $ (Flag,) <$> hiddenCells b p sz
  | n == count b Flagged (neighbours p sz) = Just $ (Reveal,) <$> hiddenCells b p sz
decideMove _ _ _ _ = Nothing

-- | Returns a list of all the 'Hidden' cells around a co-ordinate.
--
-- Private to the module.
hiddenCells b p sz = filter (\p -> b !? p == Hidden) (neighbours p sz)

-- | Returns a list of all the cells around a co-ordinate.
--
-- Private to the module.
neighbours p sz = clip sz $ genNeighbours p

-- | Counts the number of cells, of a given type('Numbered', 'Empty'...) in the
-- entire 'Board'.
--
-- Private to the module.
count :: Board -> Cell -> [Point] -> Int
count b c points =
  let
  matchCell n p
    | b !? p == c = succ n
    | otherwise = n
  in foldl matchCell 0 points
