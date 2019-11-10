module Mine where

import Data.Map.Strict(Map(..), empty, insert)
import Control.Effect
import Control.Effect.State
import Control.Effect

data Overlay = Hidden | Flagged deriving (Show, Eq, Ord, Enum)

data Cell
  = Empty Overlay
  | Numbered Int Overlay
  | Mine Overlay
  deriving (Show)

data Difficulty = Easy | Mid | Hard

newtype Board = Board { unBoard :: (Map (Int, Int) Cell, Int) }
deriving instance Show Board

getDifficulty :: Difficulty -> (Int, Int)
getDifficulty Easy = (8, 10)
getDifficulty Mid = (14, 50)
getDifficulty Hard = (20, 180)

createBoard :: (Carrier sig m, Effect sig) => Difficulty -> m (Board, ())
createBoard diff = runState (Board (empty,0)) $ do
    m' <- get
    let (size, mines) = getDifficulty diff
    let (m'', size') = unBoard m'
    put $ Board $ (,) (insert (size, mines) (Empty Hidden) m'') size
