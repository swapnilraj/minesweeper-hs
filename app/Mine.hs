{- | Module to represent a 'Board', contains all functions required to
    manipulate and query the 'Board'.

    'Board' is internally represented as 'Map' from 'Point' to 'Cell'.
-}
module Mine
  ( Board(..)
  , Cell(..)
  , Difficulty(..)
  , Overlay(..)
  , Point(..)
  , (!?)
  , clip
  , createBoard
  , emptyBoard
  , exploreCells
  , flagCell
  , genNeighbours
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

-- | Defines the 'Overlay' state on a 'Cell', an overlay for a cell can either
-- can be 'Open', 'Hidden' or 'Flagged'.
data Overlay = Open | Hidden | Flagged deriving (Show, Eq, Ord, Enum)

-- | Defines the overall type of cell that the 'Board' can represent.
-- This in conjunction with 'Overlay' covers all the possible states of a
-- Minesweeper cell.
-- A cell can either be 'Empty', 'Numbered' or a 'Mine'.
data Cell
  = Empty Overlay
  | Numbered Int Overlay
  | Mine Overlay
  deriving (Eq, Ord)

-- | Show instance for a 'Cell', used in the show instance for a 'Board'
--
-- All 'Hidden' cells are represented by a space.
--
-- All 'Flagged' cells are represented by a '?'.
--
-- Open Empty cell is "O".
--
-- Open Numbred cell is the number it holds.
--
-- Open Mine is "M".
instance Show Cell where
  show c
    | isHiddenCell c = " "
    | isFlaggedCell c = "?"
    | otherwise = case c of
                    Empty{} -> "O"
                    Numbered n _ -> show n
                    Mine{} -> "M"

-- | Type for a co-ordinate on the 'Board'
type Point = (Int, Int)
-- | Utility function to make a 'Board'.
--
-- Opposite of 'unBoard'.
mkBoard :: Map Point Cell -> Int -> Board
mkBoard m s = Board $ (,) m s
-- | An empty board, used as the initial state for 'createBoard'.
emptyBoard = mkBoard empty 0
type Size = Int
-- | 'Board' represents a Minesweeper board
--
-- I chose a 'Map' for the 'Board', since it gives fast insertion and deletion
-- compared to a Vector, List or a Matrix, insertion and deletion would take
-- O(n^2).
--
-- The 'Board' also stores the 'Size' for convenience for other functions likes
-- rendering, and clipping neighbours.
newtype Board = Board { unBoard :: (Map Point Cell, Size) }

-- | 'Show' instance for 'Board', doesn't explicitly show the 'Size' but that
-- can be inferred from the number of columns showed.
instance Show Board where
  show b = let (board, size) = unBoard b
               points = [ [ (x, y) |  x <- [0..size-1] ] | y <- [0..size-1]]
               showRow row =
                 let c p = (!?) board p
                     f x ='|':show x
                  in concat $ f <$> (c <$> row)
            in unlines $ showRow <$> points

-- | Type to represent the 'Difficulty' of a game.
--
-- The 'Difficulty' dictates the mine ratio, i.e. the size of the 'Board' and
-- the number of Mines.
data Difficulty = Easy | Mid | Hard

-- | An interpretation for the 'Difficulty', in this case
--
-- +--------------+--------+---------+
-- | 'Difficulty' | 'Size' | # Mines |
-- +==============+========+=========+
-- |     Easy     |   8x8  |   10    |
-- +--------------+--------+---------+
-- |    Medium    | 16x16  |   40    |
-- +--------------+--------+---------+
-- |     Hard     | 24x24  |   99    |
-- +--------------+--------+---------+
getDifficulty :: Difficulty -> (Int, Int)
getDifficulty Easy = (8, 10)
getDifficulty Mid = (16, 40)
getDifficulty Hard = (24, 99)

-- | Override the infix lookup function from 'Map', instead of returning a
-- 'Maybe Cell', the 'Nothing' case means its a 'Empty Hidden' cell so it uses
-- 'findWithDefault' and replaces 'Nothing' with 'Empty Hidden'
(!?) :: Map Point Cell -> Point -> Cell
(!?) m k = findWithDefault (Empty Hidden) k m

-- | Generates the co-ordinates of the neighbours for a give 'Point'.
genNeighbours :: (Enum a, Enum b) => (a, b) -> [(a, b)]
genNeighbours (x, y) = [ (pred x, pred y)
                    , (x, pred y)
                    , (succ x, pred y)
                    , (pred x, y)
                    , (succ x, y)
                    , (pred x, succ y)
                    , (x, succ y)
                    , (succ x, succ y)
                    ]

-- | Clips a list of 'Point' to make sure all co-ordinates lie inside the
-- 'Board'.
clip :: (Num a, Ord a) => a -> [(a, a)] -> [(a, a)]
clip size = let inBounds (x, y) = x < size && y < size && x >= 0 && y >= 0
              in filter inBounds

-- | Creates a 'Board', with randomly placed mines and 'Numbered' cells placed
-- on the neighbours of a 'Mine'.
--
-- The implementation places a 'Mine', and then increments the count of
-- 'Numbered' cell around the 'Mine'.
--
-- Maintains the 'Board' as local state.
--
-- Returns a list of the locations, of the mines.
createBoard :: Difficulty -> StateT Board IO [Point]
createBoard diff = do
    let (size, mines) = getDifficulty diff
    mineLoc <- liftIO $ genMines size mines
    forM_ mineLoc $ \loc -> do
      board_ <- get
      let (board', _) = unBoard board_
      put $ mkBoard (insert loc (Mine Hidden) board') size
      placeNumbers loc size
    pure mineLoc
  where
    placeNumbers loc size = do
      forM_ (clip size (genNeighbours loc)) $ \num -> do
        board_ <- get
        let (board', _) = unBoard board_
        put $ mkBoard (placeNumber board' num) size

-- | Increments the number for a 'Numbered' celll.
--
-- Places a 'Numbered' cell with a count of 1, on an 'Empty' cell.
--
-- Ignores if the 'Point' has a 'Mine' on it.
--
-- Private to the module.
placeNumber :: Map Point Cell -> Point -> Map Point Cell
placeNumber board loc = case (board !? loc) of
                        Empty{} -> insert loc (Numbered 1 Hidden) board
                        (Numbered n _) -> insert loc (Numbered (succ n) Hidden) board
                        Mine{} -> board

-- | Generates a list of random 'Point', and returns a unique list of 'Point'.
--
-- Guarantees that the length of the list is greater or equal to 'n'
--
-- Private to the module.
genMines :: Int -> Int -> IO [(Int, Int)]
genMines size n = nub <$> (sequence $ replicate n $ tups)
  where tups = (,) <$> randomRIO (0, size - 1) <*> randomRIO (0, size - 1)

-- | Flags a cell, changes the 'Overlay' state of a 'Cell'.
--
-- Can only flag, 'Hidden' cells
-- If an attempt is made to flag a cell, a flagged cell then the cell 'Overlay'
-- state is changed to 'Hidden'.
--
-- All other states are ignored and the function acts like 'id'.
flagCell :: Cell -> Cell
flagCell (Empty Hidden) = Empty Flagged
flagCell (Numbered n Hidden) = Numbered n Flagged
flagCell (Mine Hidden) = Mine Flagged
flagCell (Empty Flagged) = Empty Hidden
flagCell (Numbered n Flagged) = Numbered n Hidden
flagCell (Mine Flagged) = Mine Hidden
flagCell x = x

-- | Attempts to reveal a 'Cell', only reveals 'Hidden' cells and acts like 'id'
-- for other cells.
--
-- Private to the module.
revealCell :: Point -> Cell -> Cell
revealCell loc (Empty Hidden) = Empty Open
revealCell loc (Numbered n Hidden) = Numbered n Open
revealCell loc (Mine Hidden) = Mine Open
revealCell loc x = x

-- | Reveal 'Hidden' 'Cell's, till a boundary of 'Numbered' cells is reached.
exploreCells :: [Point] -- ^ Neighbours to explore
             -> S.Set Point -- ^ Cells that are already visited
             -> Board -- ^ Current 'Board'
             -> Board -- ^ Final 'Board' with appropriate cells revealed
exploreCells [] _ b = b
exploreCells (p:ps) visited b' =
  let (b, sz) = unBoard b'
      cell = b !? p
      ncell = revealCell p cell
      visited' = S.insert p visited
      neighboursToExplore = filter (flip S.notMember visited') (clip sz (genNeighbours p))
      board' = mkBoard (insert p ncell b) sz
      in
    case (cell) of
      Empty{} -> exploreCells (ps ++ neighboursToExplore) visited' board'
      _ -> exploreCells ps visited' board'

-- | Counts the number of 'Hidden' cells in a 'Board'.
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

-- | Counts the number of 'Mine's in a 'Board'.
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

-- | Returns 'True' for a 'Hidden' cell and 'False' for all else.
isHiddenCell :: Cell -> Bool
isHiddenCell (Empty Hidden) = True
isHiddenCell (Numbered _ Hidden) = True
isHiddenCell (Mine Hidden) = True
isHiddenCell _ = False

-- | Returns 'True' for a 'Flagged cell and 'False' for all else.
isFlaggedCell :: Cell -> Bool
isFlaggedCell (Empty Flagged) = True
isFlaggedCell (Numbered _ Flagged) = True
isFlaggedCell (Mine Flagged) = True
isFlaggedCell _ = False
