module AI
  ( AIMove
  , solve
  ) where

import AIMine
  ( Board(..)
  , Cell(..)
  , Point(..)
  , (!?)
  , allHidden
  , assocs
  , elems
  , size
  )

import Gameplay(Move(..))
import Z3.Monad
import Control.Monad(forM_)

type AIMove = (Move, Point)
mkMove m l = (,) m l

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

points size= [ [ (x, y) |  x <- [0..size-1] ] | y <- [0..size-1]]

solve :: Board -> Either [AIMove] AIMove
solve b
  -- If all cells are closed guess an edge cell probability tells us that an
  -- edge cell is least likely to have a mine
  | allHidden b = Right $ mkMove Reveal (0, 0)
  | otherwise = do
    let sz = size b
    -- find flags
        onlyFlags = filter ()
        case (length onlyFlags == 0) of
          True -> Left $ []
          -- choice <|>
          Flase -> decideMove b p

-- Maybe
decideMove b p (Numbered n)
  | n == count b Hidden $ clip sz $ genNumbers p = -- Flag
  | n == count b Flagged $ clip sz $ generated p = -- Reveal
devideMove _ _ _ = Nothing

count :: Board -> [Point] -> Cell -> Int
count b points c =
  let
  matchCell n p
    | b !? p == c = succ n
    | otherwise = n
  in foldl matchCell 0 points


cell :: MonadZ3 z3 => Int -> Int -> z3 AST
cell x sz
  | x == 0 || x == (sz + 1) = mkFreshIntVar $ show x
  | otherwise = mkFreshIntVar $ show (x+1)

script :: Z3 (Maybe [Integer])
script = undefined
