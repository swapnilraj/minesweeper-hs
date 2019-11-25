{- | Module responsible handling all rendering and uesr input related things.

    Also renders helpful messages for the user, like rendering when a 'Mine' is
    revealed, and when a game is won.

    The 'Help AI!' button tries to make a move or displays a message stating
    that it can't make a move at the momemnt.
-}
module Graphics
  ( setup
  ) where

import Control.Monad (forM_, join, void)
import Control.Monad.Trans.State(StateT(..), execStateT, get, put, runStateT)
import Control.Concurrent(threadDelay)
import Data.IORef(newIORef, readIORef, writeIORef)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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

-- | Applies a function to both elements in a tuple.
--
-- Private to the module.
both f (x, y) = (f x, f y)
-- | Colors used for the rendering.
--
-- Private to the module.
tileColor, backgroundColor, pageBackground, textColor :: String
tileColor = "#5F2AAF"
backgroundColor = "#C3A6F7"
pageBackground = "#E9DEFC"
textColor = pageBackground

-- | Size of canvas
--
-- Private to the module.
canvasSize :: Int
canvasSize = 768
second = 1000000

-- | Function responsible for setupping all input events and rendering the
-- board.
setup :: Window -> UI ()
setup w = void $ do
  (mineLoc, randBoard) <- liftIO $ runStateT (createBoard Mid) emptyBoard

  boardRef <- liftIO $ newIORef randBoard
  mineLocRef <- liftIO $ newIORef mineLoc

  board <- liftIO $ readIORef boardRef
  let numMines' = numMines board

  pure w # set title "Minesweeper"
  bodys' <- getElementsByTagName w "body"
  head bodys' # set' style [("background", pageBackground)]

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set style [("background", backgroundColor)]

  drawBoard canvas board numMines'

  easyBtn <- UI.button # set UI.text "Easy"
  mediumBtn <- UI.button # set UI.text "Medium"
  hardBtn <- UI.button # set UI.text "Hard"
  aiBtn <- UI.button # set UI.text "Help AI!!"
  aiMsg <- UI.paragraph

  let
    -- | Renders a message when the user has lost, i.e. revealed a mine.
    -- Sets up a new a board after a second.
    loseMessage :: String -> UI ()
    loseMessage a = do
      mkText' a canvas (100, 100)
      liftIO $ threadDelay second
      newBoardButton Easy

    -- | Creates and renders a new random board, based on the passed in
    -- 'Difficulty'.
    newBoardButton diff = do
      (minLoc, b) <- liftIO $ runStateT (createBoard diff) emptyBoard
      liftIO $ writeIORef mineLocRef mineLoc
      liftIO $ writeIORef boardRef b
      b' <- liftIO $ readIORef boardRef
      let (_, sz) = unBoard b'
      UI.clearCanvas canvas
      drawBoard canvas b' numMines'

    -- | Generic handler for a click, takes a 'Move' and a 'Point'.
    click move point = do
      b <- liftIO $ readIORef boardRef
      case (stepBoard move point b) of
        Left (err, b) -> do
          liftIO $ writeIORef boardRef b
          b' <- liftIO $ readIORef boardRef
          drawBoard canvas b' (numMines b')
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
        Nothing -> do
          aiMsg # set' UI.text "No move for now! I'll be back"
          liftIO $ threadDelay second
          aiMsg # set' UI.text ""
        Just [] -> do
          aiMsg # set' UI.text "No move for now! I'll be back"
          liftIO $ threadDelay second
          aiMsg # set' UI.text ""
        Just ((m, loc):_) -> do
          click m loc

  getBody w #+ [ column [ element canvas ]
               , element easyBtn
               , element mediumBtn
               , element hardBtn
               , element aiBtn
               , element aiMsg
               ]

-- | Draw function for a 'Board'
--
-- 'Threepenny' should have made a typeclass for Diplay.
--
-- Private to the module.
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

-- | Function to draw a 'Cell'
--
-- A 'Hidden' 'Cell' is rendered as an empty block of color 'tileColor'
--
-- A 'Flagged' 'Cell' is rendered as a "?"
--
-- An 'Open' 'Mine' is rendered as block of color 'backgroundColor'
--
-- An 'Open 'Numbered' is rendered as the number itself.
--
-- Am 'Open' Mine is rendered as the letter "M".
--
-- Private to the module.
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
      cv # UI.fillText txt (x+(cellSize / 2)-10,y+((cellSize/2))+12)
