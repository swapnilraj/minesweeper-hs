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
  , size
  )

import Gameplay(Move(..))

type AIMove = (Move, Point)
mkMove m l = (,) m l

solve :: Board -> Either [AIMove] AIMove
solve b
  -- If all cells are closed guess an edge cell probability tells us that an
  -- edge cell is least likely to have a mine
  | allHidden b = Right $ mkMove Reveal (0, 0)
  | otherwise = undefined
