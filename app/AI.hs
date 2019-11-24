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

solve :: Board -> Either [AIMove] AIMove
solve b
  -- If all cells are closed guess an edge cell probability tells us that an
  -- edge cell is least likely to have a mine
  | allHidden b = Right $ mkMove Reveal (0, 0)
  | otherwise = do
    let sz = size b
    -- find flags
        onlyFlags = filter ()
        case (length onlyFlags == 0) of
          True -> Left $ []
          -- choice <|>
          Flase -> decideMove b p

-- Maybe
decideMove b p (Numbered n)
  | n == count b Hidden $ clip sz $ genNumbers p = -- Flag
  | n == count b Flagged $ clip sz $ generated p = -- Reveal
devideMove _ _ _ = Nothing

count :: Board -> [Point] -> Cell -> Int
count b points c =
  let
  matchCell n p
    | b !? p == c = succ n
    | otherwise = n
  in foldl matchCell 0 points
