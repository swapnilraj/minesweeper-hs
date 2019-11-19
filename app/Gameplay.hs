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
  , numMines
  , numUnOpenedCells
  )

data Move = Flag | Reveal deriving (Show, Eq, Ord, Read)

stepBoard :: Move -> Point -> Board -> Either String Board
stepBoard move loc b =
  let (b', sz) = unBoard b
      val = b' !? loc
      flag = Right $ mkBoard (insert loc (flagCell val) b') sz
      reveal = case val of
                 Mine{} -> Left $ "Oops! That was a mine"
                 _ -> Right $ exploreCells [loc] S.empty b
   in
      case move of
        Flag -> flag
        Reveal -> reveal

gamePlay :: StateT Board IO ()
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
    Left e -> liftIO $ print e
    Right b -> put b
  gamePlay

