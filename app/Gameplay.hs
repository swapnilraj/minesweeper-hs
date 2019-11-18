module Gameplay
  ( Move(..)
  , stepBoard
  ) where

import Data.Map.Strict(Map(..), empty, insert)
import Control.Monad.Trans.State(StateT(..), runStateT, get, put)

import Mine
  ( Board(..)
  , Cell(..)
  , Overlay(..)
  , Point(..)
  , (!?)
  , mkBoard
  , flagCell
  , revealCell
  )

data Move = Flag | Reveal deriving (Show, Eq, Ord, Enum)

stepBoard :: Point -> Move -> Board -> Either String Board
stepBoard loc move b =
  let (b', sz) = unBoard b
      val = b' !? loc
      flag = Right $ mkBoard (insert loc (flagCell val) b') sz
      reveal = case val of
                 Mine{} -> Left $ "Error"
                 _ -> Right $ mkBoard (insert loc (revealCell val) b') sz
   in
      case move of
        Flag -> flag
        Reveal -> reveal

gamePlay :: StateT Board IO ()
gamePlay = undefined
