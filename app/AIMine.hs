{-|
  An indirection to not expose any of the information that the AI can use to
  solve the board.
  The exports from this module, make the board look like as a human is looking
  at the rendered version of the world.
  The AI only needs to read the board, it doesn't need to write.

  This module is our last hope, skynet is alive...
-}
module AIMine
  ( M.Board
  -- | Re-export type from module "Mine"
  , Cell(..)
  , M.Point(..)
  -- | Re-export type from module "Mine"
  , (!?)
  , allHidden
  , assocs
  , M.clip
  -- | Re-export utility function from module "Mine"
  , M.genNeighbours
  -- | Re-export utility function from module "Mine"
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

-- | Too confusing if its the same name? This represents a cell from a human's
-- perspective. The AI should have the same information as a human who looks at
-- a rendered board.
data Cell
  = Hidden -- ^ An unrevealed cell
  | Empty -- ^ A revealed cell, which is empty
  | Flagged -- ^ An unrevealed cell, with a flag on it
  | Numbered Int -- ^ A cell adjacent to a mine, where the number represents
  --how many mines there are close to it
  deriving (Eq, Show)

-- | The size of the board, the length of the board to be more precise; a board
-- is guaranteed to be a square. Utility function to avoid exposing the
-- constructor for 'M.Board' to "AI".
size :: M.Board -> Int
size b = snd $ M.unBoard b

-- | Checks if the all the cells in a board are 'Hidden'
-- Utility function to exposing the constructor for 'M.Board' to "AI".
allHidden :: M.Board -> Bool
allHidden b' = let (b, _) = M.unBoard b' in
                   Map.foldl (\acc val -> acc && (humanVision val == Hidden))
                          True b

-- | Similar to 'Map.assocs' but for 'M.Board' type.
-- Utility function to exposing the constructor for 'M.Board' to "AI".
assocs :: M.Board -> [(M.Point, Cell)]
assocs b' = let (b, _) = M.unBoard b'
             in (\(p, c) -> (p, humanVision c)) <$> Map.assocs b

-- | Applies human vision to the underlying 'Map' in 'M.Board'.
-- For examples turns a value of 'M.Empty Hidden' turns to 'Hidden'.
-- Errors out if a 'M.Mine' is seen as the AI should never be invoked in a case
-- where a mine was exposed.
-- This function is the main purpose of the of this module.
humanVision :: M.Cell -> Cell
humanVision c
  | M.isHiddenCell c = Hidden
  | M.isFlaggedCell c = Flagged
  | otherwise = case c of
                  M.Empty{} -> Empty
                  M.Numbered n _ -> Numbered n
                  M.Mine{} -> error "Ooft! you've already lost"

-- | Override the infix lookup function exported by Data.Map.
-- As the type signature suggests, it applies 'humanVision' to value obtained
-- by looking up in the Map.
(!?) :: M.Board -> M.Point -> Cell
(!?) b p = humanVision . flip (M.!?) p $ fst $ M.unBoard b
