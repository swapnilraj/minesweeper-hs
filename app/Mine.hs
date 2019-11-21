module Mine
  ( Board(..)
  , Cell(..)
  , Difficulty(..)
  , Overlay(..)
  , Point(..)
  , (!?)
  , createBoard
  , emptyBoard
  , exploreCells
  , flagCell
  , getDifficulty
  , isFlaggedCell
  , isHiddenCell
  , mkBoard
  , numMines
  , numUnOpenedCells
  ) where

import Prelude hiding (foldl)
import Data.Map.Strict(Map(..), elems, empty, foldl, findWithDefault, insert)
import Data.List(intercalate, nub)
import qualified Data.Set as S(Set(..), insert, empty, notMember)
import Control.Monad(forM_)
import Control.Monad.Trans.State(StateT(..), execStateT, get, put)
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
emptyBoard = mkBoard empty 0
type Size = Int
newtype Board = Board { unBoard :: (Map Point Cell, Size) }

instance Show Board where
  show b = let (board, size) = unBoard b
               points = [ [ (x, y) |  y <- [0..size-1] ] | x <- [0..size-1]]
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

placeNumber :: Map Point Cell -> Point -> Map Point Cell
placeNumber board loc = case (board !? loc) of
                        Empty{} -> insert loc (Numbered 1 Hidden) board
                        (Numbered n _) -> insert loc (Numbered (succ n) Hidden) board
                        Mine{} -> board

genMines :: Int -> Int -> IO [(Int, Int)]
genMines size n = nub <$> (sequence $ replicate n $ tups)
  where tups = (,) <$> randomRIO (0, size - 1) <*> randomRIO (0, size - 1)

flagCell :: Cell -> Cell
flagCell (Empty Hidden) = Empty Flagged
flagCell (Numbered n Hidden) = Numbered n Flagged
flagCell (Mine Hidden) = Mine Flagged
flagCell (Empty Flagged) = Empty Hidden
flagCell (Numbered n Flagged) = Numbered n Hidden
flagCell (Mine Flagged) = Mine Hidden
flagCell x = x

revealCell :: Point -> Cell -> Cell
revealCell loc (Empty Hidden) = Empty Open
revealCell loc (Numbered n Hidden) = Numbered n Open
revealCell loc (Mine Hidden) = Mine Open
revealCell loc x = x

 -- Neighbours to explore   Visited        Board
exploreCells :: [Point] -> S.Set Point -> Board -> Board
exploreCells [] _ b = b
exploreCells (p:ps) visited b' =
  let (b, sz) = unBoard b'
      cell = b !? p
      ncell = revealCell p cell
      visited' = S.insert p visited
      neighboursToExplore = filter (flip S.notMember visited') (clip sz (genNumbers p))
      board' = mkBoard (insert p ncell b) sz
      in
    case (cell) of
      Empty{} -> exploreCells (ps ++ neighboursToExplore) visited' board'
      _ -> exploreCells ps visited' board'

numUnOpenedCells :: Board -> Int
numUnOpenedCells b'
  = let (b, sz) = unBoard b'
        countUnopened n cell
          | isOpen cell = n
          | otherwise = succ n
      in (foldl countUnopened 0 b) + (sz * sz - (length $ elems b))
  where
    isOpen (Empty Open) = True
    isOpen (Numbered n Open) = True
    isOpen (Mine Open) = True
    isOpen _ = False

numMines :: Board -> Int
numMines b'
  = let (b, sz) = unBoard b'
        countMines n cell
          | isMine cell = succ n
          | otherwise = n
      in foldl countMines 0 b
  where
    isMine Mine{} = True
    isMine _ = False

isHiddenCell :: Cell -> Bool
isHiddenCell (Empty Hidden) = True
isHiddenCell (Numbered _ Hidden) = True
isHiddenCell (Mine Hidden) = True
isHiddenCell _ = False

isFlaggedCell :: Cell -> Bool
isFlaggedCell (Empty Flagged) = True
isFlaggedCell (Numbered _ Flagged) = True
isFlaggedCell (Mine Flagged) = True
isFlaggedCell _ = False
