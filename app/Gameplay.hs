{-| 'Gameplay' module to progress the game play.

   Contains functions to progress the game with one 'Move', returns an 'Either'
   with 'Board' in the 'Right' constructor, and a '(String, Board)' in the
   'Left' constructor where the 'String' is the error message and 'Board' is
   the state of the 'Board' after making the move.
-}
module Gameplay
  ( Move(..)
  , stepBoard
  , gamePlay
  ) where

import Data.Map.Strict(Map(..), empty, insert)
import qualified Data.Set as S(empty)
import Control.Monad.Trans.State(StateT(..), runStateT, get, put)
import Control.Monad.Trans(liftIO)

import Mine
  ( Board(..)
  , Cell(..)
  , Overlay(..)
  , Point(..)
  , (!?)
  , exploreCells
  , flagCell
  , mkBoard
  , numUnOpenedCells
  )

-- | Represents the 'Move', can either be 'Flag' or 'Reveal' a cell.
data Move = Flag | Reveal deriving (Show, Eq, Ord, Read)

-- | Function to step the game with one 'Move'.
--
-- Used in 'Graphics' to process user click events,
stepBoard :: Move -> Point -> Board -> Either (String, Board) Board
stepBoard move loc b =
  let (b', sz) = unBoard b
      val = b' !? loc
      flag = Right $ mkBoard (insert loc (flagCell val) b') sz
      newBoard = exploreCells [loc] S.empty b
      reveal = case val of
                 Mine{} -> Left $ (,) "Oops! That was a mine" newBoard
                 _ -> Right $ newBoard
   in
      case move of
        Flag -> flag
        Reveal -> reveal

-- | Function to play the game in CLI.
--
-- First input is the 'Point' and uses the 'Read' instance to process the
-- 'Point'.
--
-- Second input is the 'Move' again the 'Read' instance to process the 'Move'.
--
-- Prints the 'Board' and the number of 'Hidden' cells at each step.
gamePlay :: Int -> StateT Board IO ()
gamePlay mines = do
  board <- get
  let unopened = numUnOpenedCells board
  liftIO $ print unopened
  liftIO $ print board
  input <- liftIO $ getLine
  let point = (read input) :: (Int, Int)
  input <- liftIO $ getLine
  let move = (read input) :: Move
  let new = stepBoard move point board
  case new of
    Left(e, b) -> (liftIO $ print e) >> put b
    Right b -> put b
  gamePlay mines

