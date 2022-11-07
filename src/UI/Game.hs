{-# LANGUAGE OverloadedStrings #-}

module UI.Game (playGame) where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
    ( vBox,
      overrideAttr,
      withBorderStyle,
      withAttr,
      vLimit,
      str,
      padTop,
      padLeft,
      hLimit,
      hBox )
import Brick.Widgets.ProgressBar
    ( progressBar, progressCompleteAttr )
import qualified Brick.Widgets.ProgressBar as P
import BB
    ( InitConfig,
      GameStatus(..),
      Direction(West, East),
      Game,
      balls,
      hardBricks,
      highestScore,
      initConfig,
      level,
      lifeCount,
      player,
      pureBricks,
      score,
      status,
      height,
      width,
      pause,
      step,
      withinPlayer,
      movePlayer,
      isHardBrick,
      initGame,
      progress,
      timeLimit,
      initTickInterval )
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.), (&), (.~))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Text.Printf (printf)
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2), Sel3 (sel3))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Player | Empty | Ball | PureBrick | HardBrick

-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

--todo: wrap init config into a class  
playGame :: InitConfig  -> IO Game
playGame initConf = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay $ initConf^.initTickInterval
  initG <- initGame initConf
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just chan) app initG

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue (if g ^. status `elem` [Playing, Paused] then movePlayer East g else g)
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue (if g ^. status `elem` [Playing, Paused] then movePlayer West g else g)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame (g ^. initConfig ) ) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue$ pause g
handleEvent g (VtyEvent V.EvLostFocus) = continue$ pause g
handleEvent g (VtyEvent (V.EvMouseDown c r button mods)) = halt g

handleEvent g _ = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.hCenter $ C.vCenter $ hBox
      [ padLeft (Pad 20) $ drawGrid g
      , padLeft (Pad 6) $ drawInfoBoard g
      ]
  ]

drawInfoBoard :: Game -> Widget Name
drawInfoBoard g = withBorderStyle BS.unicodeBold $
      vBox [
        drawStats g,
        padTop (Pad 10) drawHelp,
        padTop (Pad 4) $ drawTimeBar g
      ]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 30 $
    vBox
      [
        drawHighestScore (g^.highestScore),
        padTop (Pad 2) $ drawScore (g ^. score),
        padTop (Pad 2) $ drawLifeCount $ g ^. lifeCount,
        padTop (Pad 2) $ drawGameStatus $ g ^. status
      ]

drawScore :: Int -> Widget Name
drawScore n = withAttr noticeStringAttr $ str $ "score:" ++ show n

drawHighestScore :: Int -> Widget Name
drawHighestScore n = withAttr noticeStringAttr $ str $ "highest:" ++ show n

drawLifeCount :: Int -> Widget Name
drawLifeCount n = withAttr noticeStringAttr $ str $ "♥:" ++ show n

drawGameStatus :: GameStatus  -> Widget Name
drawGameStatus Dead = withAttr noticeStringAttr $ str "GAME OVER"
drawGameStatus Win = withAttr noticeStringAttr $ str "YOU PASSED!"
drawGameStatus Paused = withAttr noticeStringAttr $ str "MOVE LEFT/RIGHT TO RESUME"
drawGameStatus _ =  withAttr noticeStringAttr $ str " "

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str$ " Level " ++ (show $ g^.level) ++ " ") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c@(V2 x y)
      | y == 0 && withinPlayer c (g^.player) = Player
      | c `elem` ballsToCoords (g ^. balls) = Ball
      | foldl (||) False (fmap (isHardBrick c) (g ^. pureBricks))  = PureBrick
      | foldl (||) False (fmap (isHardBrick c) (g ^. hardBricks)) = HardBrick
      | otherwise = Empty

-- util
ballsToCoords :: Seq (b1, b2, c) -> Seq b1
ballsToCoords = S.mapWithIndex f
  where f _ (b, _, _) = b
--

drawTimeBar :: Game -> Widget Name
drawTimeBar g =
 overrideAttr progressCompleteAttr timeBarAttr
    $ vLimit 3 $
          hLimit 15 $
            progressBar (Just $ showProgress per) per
  where
    per = num / denom 
    num = fromIntegral (g ^. progress) :: Float
    denom = fromIntegral (g ^. timeLimit) :: Float

showProgress :: Float -> String
showProgress n = printf "Time: %.0f%%" (n * 100)

drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell Ball = withAttr ballsAttr ballw
drawCell PureBrick = withAttr pureBricksAttr cw
drawCell HardBrick = withAttr hardBricksAttr cw

drawHelp :: Widget Name
drawHelp =
           hLimit 25
             $ vBox
              $ map (\a -> withAttr noticeStringAttr $ C.hCenter $ str $ sel1 a++ sel2 a)
              [
                ("Move Left: "   , "←")
              , ("Move Right: "  , "→")
              , ("Restart: ", "r")
              , ("Quit: "   , "q")
              ]


cw :: Widget Name
cw = str "  "

ballw :: Widget Name
ballw = str "⬤ "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.blue `on` V.blue),
      (noticeStringAttr, fg V.red `V.withStyle` V.bold),
      (pureBricksAttr, V.white `on` V.white),
      (hardBricksAttr, V.yellow `on` V.yellow),
      (ballsAttr, fg V.red  `V.withStyle` V.bold),
      (timeBarAttr, V.black `on` V.blue)
    ]

noticeStringAttr :: AttrName
noticeStringAttr = "noticeString"

playerAttr :: AttrName
playerAttr = "player"

emptyAttr :: AttrName
emptyAttr = "empty"

ballsAttr :: AttrName
ballsAttr = "balls"

pureBricksAttr :: AttrName
pureBricksAttr = "pureBricks"
hardBricksAttr :: AttrName
hardBricksAttr = "hardBricks"

timeBarAttr :: AttrName
timeBarAttr = "timeBar"

