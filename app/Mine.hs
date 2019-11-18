module Mine
  ( Board(..)
  , Cell(..)
  , Difficulty(..)
  , Overlay(..)
  , Point(..)
  , (!?)
  , createBoard
  , emptyBoard
  , mkBoard
  , flagCell
  , getDifficulty
  , revealCell
  ) where

import Data.Map.Strict(Map(..), findWithDefault, empty, insert)
import Data.List(intercalate, nub)
import Control.Monad(forM_)
import Control.Monad.Trans.State(StateT(..), runStateT, get, put)
import Control.Monad.Trans(liftIO)
import System.Random(randomRIO)

data Overlay = Open | Hidden | Flagged deriving (Show, Eq, Ord, Enum)

data Cell
  = Empty Overlay
  | Numbered Int Overlay
  | Mine Overlay
  deriving (Eq, Ord)

instance Show Cell where
  show (Empty Hidden) = " "
  show (Numbered _ Hidden) = " "
  show (Mine Hidden) = " "
  show (Empty Open) = "O"
  show (Numbered n Open) = show n
  show (Mine Open) = "M"
  show _ = "?"

type Point = (Int, Int)
mkBoard :: Map Point Cell -> Int -> Board
mkBoard m s = Board $ (,) m s
emptyBoard = Board (empty, 0)
newtype Board = Board { unBoard :: (Map Point Cell, Int) }

instance Show Board where
  show b = let (board, size) = unBoard b
               rows = \c -> [ (c, r) | r <- [0..size-1] ]
               points = rows <$> [0..size-1]
               showRow row =
                 let c = (!?) board
                     f x = '|':show x
                  in concat $ f <$> (c <$> row)
            in unlines $ showRow <$> points

data Difficulty = Easy | Mid | Hard

getDifficulty :: Difficulty -> (Int, Int)
getDifficulty Easy = (8, 10)
getDifficulty Mid = (16, 40)
getDifficulty Hard = (24, 99)

(!?) :: Map Point Cell -> Point -> Cell
(!?) m k = findWithDefault (Empty Hidden) k m

genNumbers :: (Enum a, Enum b) => (a, b) -> [(a, b)]
genNumbers (x, y) = [ (pred x, pred y)
                    , (x, pred y)
                    , (succ x, pred y)
                    , (pred x, y)
                    , (succ x, y)
                    , (pred x, succ y)
                    , (x, succ y)
                    , (succ x, succ y)
                    ]

clip :: (Num a, Ord a) => a -> [(a, a)] -> [(a, a)]
clip size = let inBounds (x, y) = x < size && y < size && x >= 0 && y >= 0
              in filter inBounds

createBoard :: Difficulty -> StateT Board IO ()
createBoard diff = do
    let (size, mines) = getDifficulty diff
    mineLoc <- liftIO $ genMines size mines
    forM_ mineLoc $ \loc -> do
      board_ <- get
      let (board', _) = unBoard board_
      put $ mkBoard (insert loc (Mine Hidden) board') size
      placeNumbers loc size
  where
    placeNumbers loc size = do
      forM_ (clip size (genNumbers loc)) $ \num -> do
        board_ <- get
        let (board', _) = unBoard board_
        put $ mkBoard (placeNumber board' num) size

placeNumber :: Ord k => Map k Cell -> k -> Map k Cell
placeNumber :: Map Point Cell -> Point -> Map Point Cell
placeNumber board loc = case (board !? loc) of
                        Empty{} -> insert loc (Numbered 1 Hidden) board
                        (Numbered n _) -> insert loc (Numbered (succ n) Hidden) board
                        Mine{} -> board

genMines :: Int -> Int -> IO [(Int, Int)]
genMines size n = nub <$> (sequence $ replicate n $ tups)
  where tups = (,) <$> randomRIO (0, size - 1) <*> randomRIO (0, size - 1)
