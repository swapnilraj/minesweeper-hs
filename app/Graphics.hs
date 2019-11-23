module Graphics
  ( setup
  ) where

import Control.Monad (forM_, join, void)
import Control.Monad.Trans.State(StateT(..), execStateT, get, put, runStateT)
import Data.IORef(newIORef, readIORef, writeIORef)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Debug.Trace

import AI
  ( AIMove
  , solve
  )

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

tileColor = "#5F2AAF"
backgroundColor = "#C3A6F7"
pageBackground = "#E9DEFC"
textColor = pageBackground

canvasSize :: Int
canvasSize = 768

setup :: Window -> UI ()
setup w = void $ do
  randBoard' <- liftIO $ runStateT (createBoard Mid) emptyBoard
  let (mineLoc, randBoard) = randBoard'

  boardRef <- liftIO $ newIORef randBoard
  mineLocRef <- liftIO $ newIORef mineLoc

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

  easyBtn <- UI.button #+ [ string "Easy" ]
  mediumBtn <- UI.button #+ [ string "Medium" ]
  hardBtn <- UI.button #+ [ string "Hard" ]
  aiBtn <- UI.button #+ [ string "Help AI!!" ]

  let
    loseMessage :: String -> UI ()
    loseMessage a = mkText' a canvas (100, 100)

    newBoardButton diff = do
      boardWithMines <- liftIO $ runStateT (createBoard diff) emptyBoard
      let (mineLoc, b) = boardWithMines
      liftIO $ writeIORef mineLocRef mineLoc
      liftIO $ writeIORef boardRef b
      b' <- liftIO $ readIORef boardRef
      let (_, sz) = unBoard b'
      UI.clearCanvas canvas
      drawBoard canvas b' numMines'

    click move point = do
      b <- liftIO $ readIORef boardRef
      case (stepBoard move point b) of
        Left (err, b) -> do
          (liftIO $ writeIORef boardRef b)
          loseMessage err -- Force evaluation
        Right b -> do
          liftIO $ writeIORef boardRef b
          b' <- liftIO $ readIORef boardRef
          drawBoard canvas b' (numMines b')

  on UI.contextmenu canvas
    $ \(x,y) -> do
        b <- liftIO $ readIORef boardRef
        let (_, sz) = unBoard b
            cellsize = fromIntegral $ (canvasSize `div` sz)
            p = both (`div` (cellsize)) (x - 10, y - 10)
        click Flag p

  on UI.mouseup canvas
    $ \(x, y) -> do
        b <- liftIO $ readIORef boardRef
        let (_, sz) = unBoard b
            cellsize = fromIntegral $ (canvasSize `div` sz)
            p = both (`div` (cellsize)) (x, y)
        click Reveal p

  on UI.click easyBtn $ const $ newBoardButton Easy

  on UI.click mediumBtn $ const $ newBoardButton Mid

  on UI.click hardBtn $ const $ newBoardButton Hard

  on UI.click aiBtn $ const $
    do
      b <- liftIO $ readIORef boardRef
      case (solve b) of
        Left{} -> pure ()
        Right(m, loc) -> click m loc

  getBody w #+ [ column [ element canvas ]
               , element easyBtn
               , element mediumBtn
               , element hardBtn
               , element aiBtn
               ]

drawBoard :: Element -> Board -> Int -> UI ()
drawBoard cv b mines
  | mines == numUnOpenedCells b = mkText' "You win!" cv (100,100)
  | otherwise =
    let (board, sz) = unBoard b in
        forM_ [ [ (x, y) |  x <- [0..sz-1] ] | y <- [0..sz-1] ] $ \row ->
          forM_ row $ \point -> drawCell cv (board !? point) point sz

mkText' :: String -> Element -> UI.Point -> UI ()
mkText' txt cv (x, y) = do
  cv # set' UI.fillStyle (UI.htmlColor textColor)
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
              cv # set' UI.strokeStyle "gray"
              cv # UI.fillRect (toPoint (cellSize + 10) p) cellSize cellSize
  where
    cellSize = fromIntegral $ (canvasSize `div` sz) - 10
    toPoint :: Double -> (Int, Int) -> (Double, Double)
    toPoint pad p = both (\x -> pad * (fromIntegral x)) p
    mkText :: String -> Element -> UI.Point -> UI ()
    mkText txt cv (x, y) = do
      cv # set' UI.fillStyle (UI.htmlColor textColor)
      cv # set' UI.textFont ((show cellSize) ++ "px sans-serif")
      cv # UI.fillText txt (x+(cellSize / 2)-10,y+((cellSize/2)))
