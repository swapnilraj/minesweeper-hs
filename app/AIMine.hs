{-
  An indirection to not expose any of the information that the AI can use to
  solve the board.
  The exports from this module, make the board look like as a human is looking
  at the rendered version of the world.
  The AI only needs to read the board, it doesn't need to write.

  This module is our last hope, skynet is alive...
-}
module AIMine
  ( M.Board
  , Cell(..)
  , M.Point(..)
  , (!?)
  , allHidden
  , assocs
  , M.clip
  , M.genNeighbours
  , size
  ) where

import qualified Mine as M
  ( Board(..)
  , Cell(..)
  , Point(..)
  , (!?)
  , clip
  , isFlaggedCell
  , isHiddenCell
  , genNeighbours
  )

import Prelude hiding(foldl)
import qualified Data.Map.Strict as Map(assocs, foldl)
import Debug.Trace

data Cell -- Too confusing if its the same name?
  = Hidden
  | Empty
  | Flagged
  | Numbered Int
  deriving (Eq)

size :: M.Board -> Int
size b = snd $ M.unBoard b

allHidden :: M.Board -> Bool
allHidden b' = let (b, _) = M.unBoard b' in
                   Map.foldl (\acc val -> acc && (humanVision val == Hidden))
                          True b

assocs :: M.Board -> [(M.Point, Cell)]
assocs b' = let (b, _) = M.unBoard b'
             in (\(p, c) -> (p, humanVision c)) <$> Map.assocs b

-- count :: M.Board -> Cell -> Int
-- count b' c
--   = let (b, sz) = M.unBoard b'
--         countCells n cell
--           | cell == c = succ n
--           | otherwise = n
--       in Map.foldl countCells 0 b

humanVision :: M.Cell -> Cell
humanVision c
  | M.isHiddenCell c = Hidden
  | M.isFlaggedCell c = Flagged
  | otherwise = case c of
                  M.Empty{} -> Empty
                  M.Numbered n _ -> Numbered n
                  M.Mine{} -> error "Ooft! you've already lost"

(!?) :: M.Board -> M.Point -> Cell
(!?) b p = humanVision . flip (M.!?) p $ fst $ M.unBoard b
