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
  ) where

import qualified Mine as M
  ( Board(..)
  , Cell(..)
  , Point(..)
  , (!?)
  , isFlaggedCell
  , isHiddenCell
  )

data Cell -- Too confusing if its the same name?
  = Hidden
  | Empty
  | Flagged
  | Numbered Int

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
