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

type AIMove = (Move, Point)
mkMove m l = (,) m l

points size= [ [ (x, y) |  x <- [0..size-1] ] | y <- [0..size-1]]

isNumbered (Numbered _) = True
isNumbered _ = False

solve :: Board -> Maybe [AIMove]
solve b
  -- If all cells are closed guess an edge cell probability tells us that an
  -- edge cell is least likely to have a mine
  | allHidden b = Just $ [ mkMove Reveal (0, 0) ]
  | otherwise =
    let sz = size b
        onlyNumbered = filter (\(_, v) -> isNumbered v) (assocs b) in
        case (length onlyNumbered == 0) of
          True -> Nothing
          False -> Just $ flattenMybLst $ (\(p, val) -> decideMove b p sz val) <$> onlyNumbered

flattenMybLst :: [ Maybe [a] ] -> [a]
flattenMybLst = foldl (\acc v -> case v of
                                   Just b -> acc ++ b
                                   Nothing -> acc
                                  ) []

decideMove :: Board -> Point -> Int -> Cell -> Maybe [AIMove]
decideMove b p sz (Numbered n)
  | n == count b Hidden (neighbours p sz) = Just $ (Flag, ) <$> hiddenCells b p sz
  | n == count b Flagged (neighbours p sz) = Just $ (Reveal, ) <$> hiddenCells b p sz
decideMove _ _ _ _ = Nothing

hiddenCells b p sz = filter (\p -> b !? p == Hidden) (neighbours p sz)
neighbours p sz = clip sz (genNeighbours p)

count :: Board -> Cell -> [Point] -> Int
count b c points =
  let
  matchCell n p
    | b !? p == c = succ n
    | otherwise = n
  in foldl matchCell 0 points
