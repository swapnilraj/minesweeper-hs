module Main where

import Control.Monad.Trans.State(StateT(..), execStateT, get, put)

import Mine
  ( Difficulty(..)
  , Board(..)
  , (!?)
  , createBoard
  , emptyBoard
  , getDifficulty
  )
import Gameplay
  ( Move(..)
  , gamePlay
  )

main = do
  randBoard <- execStateT (createBoard Easy) emptyBoard
  execStateT gamePlay randBoard
