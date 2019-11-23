module AI
  ( solve
  ) where

import AIMine
  ( Board(..)
  , Cell(..)
  , Point(..)
  , (!?)
  , allClosed
  , size
  )

solve :: Board -> Either [Point] Point
solve b
  | allClosed b = Right (0, 0) -- If all cells are closed guess an edge cell
                               -- probability tells us that an edge cell is
                               -- least likely to have a mine
  | otherwise = undefined
