module Graphics
  ( setup
  ) where

import Control.Monad (forM_, void)
import Control.Monad.Trans.State(StateT(..), execStateT, get, put)
import Data.IORef(newIORef, readIORef, writeIORef)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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
  )

import Gameplay
  ( Move(..)
  , gamePlay
  )

both f (x, y) = (f x, f y)

tileColor = "#484C6B"
backgroundColor = "#5B6290"
pageBackground = "#7B8C95"

canvasSize :: Int
canvasSize = 768

setup :: Window -> UI ()
setup w = void $ do
  randBoard <- liftIO $ execStateT (createBoard Easy) emptyBoard
  boardRef <- liftIO $ newIORef $ randBoard

  pure w # set title "Minesweeper"
  bodys' <- getElementsByTagName w "body"
  let body = head bodys'
  (pure body) # set style [("background", pageBackground)]

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set style [("background", backgroundColor)]

  board <- liftIO $ readIORef boardRef
  drawBoard canvas board

  getBody w #+ [element canvas]

drawBoard :: Element -> Board -> UI ()
drawBoard cv b =
  let (board, sz) = unBoard b in
  forM_ [ [ (x, y) |  y <- [0..sz-1] ] | x <- [0..sz-1] ] $ \row ->
    forM_ row $ \point -> drawCell cv (board !? point) point sz

drawCell :: Element -> Cell -> (Int, Int) -> Int -> UI ()
drawCell cv cell p sz
      | isHiddenCell cell = do
        cv # set' UI.fillStyle (UI.htmlColor tileColor)
        cv # UI.fillRect (toPoint (cellSize + 10) p) cellSize cellSize
      | isFlaggedCell cell = do mkText "?" cv $ toPoint (cellSize + 10) p
      | otherwise = case cell of
                      (Numbered n _) -> do
                        mkText (show n) cv $ toPoint (cellSize + 10) p
                      Mine{} -> trace "WHAT" $ mkText "M" cv $ toPoint (cellSize + 10) p
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
