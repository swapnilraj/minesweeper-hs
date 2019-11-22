module Graphics
  ( setup
  ) where

import Control.Monad (forM_, join, void)
import Control.Monad.Trans.State(StateT(..), execStateT, get, put)
import Data.IORef(newIORef, readIORef, writeIORef)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Debug.Trace

import Mine
  ( Board(..)
  , Cell(..)
  , Difficulty(..)
  , (!?)
  , createBoard
  , emptyBoard
  , getDifficulty
  , isFlaggedCell
  , isHiddenCell
  , numMines
  , numUnOpenedCells
  )

import Gameplay
  ( Move(..)
  , gamePlay
  , stepBoard
  )

both f (x, y) = (f x, f y)

tileColor = "#484C6B"
backgroundColor = "#5B6290"
pageBackground = "#7B8C95"

canvasSize :: Int
canvasSize = 768

setup :: Window -> UI ()
setup w = void $ do
  randBoard <- liftIO $ execStateT (createBoard Mid) emptyBoard
  boardRef <- liftIO $ newIORef randBoard

  board <- liftIO $ readIORef boardRef
  let numMines' = numMines board

  pure w # set title "Minesweeper"
  bodys' <- getElementsByTagName w "body"
  let body = head bodys'
  (pure body) # set style [("background", pageBackground)]

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set style [("background", backgroundColor)]

  drawBoard canvas board numMines'

  newGame <- UI.button #+ [ string "New Game" ]

  let
    cellSize = fromIntegral $ canvasSize `div` 8 - 10

    loseMessage :: String -> UI ()
    loseMessage a = mkText' a canvas (100, 100)

    click move point = do
      b <- liftIO $ readIORef boardRef
      case (stepBoard move point b) of
        Left (err, b) -> do
          (liftIO $ writeIORef boardRef b)
          loseMessage err -- Force evaluation
        Right b -> do
          trace (show b) $ liftIO $ writeIORef boardRef b
          b <- liftIO $ readIORef boardRef
          drawBoard canvas b numMines'

  on UI.contextmenu canvas
    $ (click Flag)
    . both (`div` (cellSize + 10))

  on UI.mouseup canvas
    $ (click Reveal)
    . both (`div` (cellSize + 10))

  on UI.click newGame
    $ const
    $ (liftIO . join
    $ (writeIORef boardRef) <$> (execStateT (createBoard Easy) emptyBoard))
      >> drawBoard canvas board numMines'

  getBody w #+ [ element canvas
               , element newGame
               ]

drawBoard :: Element -> Board -> Int -> UI ()
drawBoard cv b mines
  | mines == numUnOpenedCells b = trace "You win" $ mkText' "You win!" cv (100,100)
  | otherwise =
    let (board, sz) = unBoard b in
        forM_ [ [ (x, y) |  y <- [0..sz-1] ] | x <- [0..sz-1] ] $ \row ->
          forM_ row $ \point -> drawCell cv (board !? point) point sz

mkText' :: String -> Element -> UI.Point -> UI ()
mkText' txt cv (x, y) = do
  cv # set' UI.fillStyle (UI.htmlColor "black")
  cv # set' UI.textFont "40px sans-serif"
  cv # UI.fillText txt (x, y)

drawCell :: Element -> Cell -> (Int, Int) -> Int -> UI ()
drawCell cv cell p sz
      | isHiddenCell cell = do
        cv # set' UI.fillStyle (UI.htmlColor tileColor)
        cv # UI.fillRect (toPoint (cellSize + 10) p) cellSize cellSize
      | isFlaggedCell cell = mkText "?" cv $ toPoint (cellSize + 10) p
      | otherwise =
          case cell of
            (Numbered n _) -> mkText (show n) cv $ toPoint (cellSize + 10) p
            Mine{} -> mkText "M" cv $ toPoint (cellSize + 10) p
            Empty{} -> do
              cv # set' UI.fillStyle (UI.htmlColor backgroundColor)
              cv # UI.fillRect (toPoint (cellSize + 10) p) cellSize cellSize
  where
    cellSize = fromIntegral $ canvasSize `div` sz - 10
    toPoint :: Double -> (Int, Int) -> (Double, Double)
    toPoint pad p = both (\x -> pad * (fromIntegral x)) p
    mkText :: String -> Element -> UI.Point -> UI ()
    mkText txt cv (x, y) = do
      cv # set' UI.fillStyle (UI.htmlColor "black")
      cv # set' UI.textFont "40px sans-serif"
      cv # UI.fillText txt (x+(cellSize / 2)-10,y+((cellSize/2)))
