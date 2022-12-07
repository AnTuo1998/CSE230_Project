{-# LANGUAGE OverloadedStrings #-}

module UI.Game (playGame) where

import BB

import qualified Graphics.Vty.Image.Internal as VI (Image (HorizText))
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
    bg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    padLeftRight,
    padTopBottom,
    str,
    strWrap,
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
  ( hBox,
    hLimit,
    overrideAttr,
    padLeft,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
  )
import Brick.Widgets.ProgressBar
  ( progressBar,
    progressCompleteAttr,
  )
import qualified Brick.Widgets.ProgressBar as P
import Buff
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2), Sel3 (sel3))
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Text.Printf (printf)
import UI.Theme (bgColor, defAttr)

-- Types

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

data Cell = Player | Empty | Ball | PureBrick | MultiLifeBrick | HardBrick | FireBallBuff | SplitBuff | FireBall_ | Bullet

-- constants

maxLevel = 5

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

playGame :: InitConfig -> IO Game
playGame initConf = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay $ initConf ^. initTickInterval
  initG <- initGame initConf
  cfg <- V.parseConfigFile "vty.config"
  let builder = V.mkVty cfg
  initialVty <- builder
  customMain initialVty builder (Just chan) app initG

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue (if g ^. status `elem` [Playing, Paused, Ready] then movePlayer East g else g)
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue (if g ^. status `elem` [Playing, Paused, Ready] then movePlayer West g else g)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame (g ^. initConfig)) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'g') [])) = if g^.level == maxLevel then continue g else halt (g & playNextLevel .~ True)
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue (if g ^. status == Playing then machineGun g else g)
handleEvent g (VtyEvent V.EvLostFocus) = continue $ pause g
handleEvent g (VtyEvent (V.EvMouseDown c r button mods)) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  continue
    ( case g ^. status of
        Paused -> resume g
        Playing -> pause g
        _ -> g
    )
handleEvent g _ = continue g


-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ withAttr globalAttr $ C.hCenter $
      C.vCenter $
        hBox
          [ padLeft (Pad 20) $ drawGrid g,
            padLeft (Pad 6) $ drawInfoBoard g
          ]
  ]

drawInfoBoard :: Game -> Widget Name
drawInfoBoard g =
  hLimit 26
    $ vBox
      [ drawStats g,
        padTop (Pad 3) drawHelp,
        padTop (Pad 3) $ drawInstr g
      ]


drawStats :: Game -> Widget Name
drawStats g = hLimit 30
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str " Stats ")
    $ vBox
      [ drawHighestScore $ g ^. highestScore,
        padTop (Pad 1) $ drawScore $ g ^. score,
        padTop (Pad 1) $ drawLifeCount (g ^. lifeCount) $ g ^. totalLifeCount
      ]



drawStat :: String -> String -> Widget Name
drawStat item stat = padLeftRight 1
                    $ withAttr noticeStringAttr
                    $ str item <+> padLeft Max (str stat)

drawScore :: Int -> Widget Name
drawScore n = drawStat "score:" $ show n


drawHighestScore :: Int -> Widget Name
drawHighestScore n = drawStat "highest:" $ show n

drawLifeCount :: Int -> Int -> Widget Name
drawLifeCount n a = drawStat "life:" $ replicate n '♥' ++ replicate (a - n) '♡'

drawGameStatus :: GameStatus -> Widget Name
drawGameStatus Dead = withAttr noticeStringAttr $ str "GAME OVER"
drawGameStatus Win = withAttr noticeStringAttr $ str "YOU PASSED!"
drawGameStatus Paused = withAttr noticeStringAttr $ strWrap "MOVE LEFT/RIGHT TO RESUME"
drawGameStatus Ready = withAttr noticeStringAttr $ strWrap "MOVE LEFT/RIGHT TO START"
drawGameStatus _ = emptyWidget


drawInstr :: Game -> Widget Name
drawInstr g = hLimit 30
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str " Instruction ")
    $ C.hCenter
    $ padTopBottom 1
    $ vBox
    [ padLeftRight 1 $ drawGameStatus $ s,
      drawTimer -- maybe combine timer with drawGameStatus
    ]
    where
      s = g ^. status
      drawTimer = if s == Playing
        then padLeftRight 1 $ drawTimeBar g
        else emptyWidget



drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str $ " Level " ++ (show $ g ^. level) ++ " ") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c@(V2 x y)
      | y == 0 && withinPlayer c (g ^. player) = Player
      | c `elem` ballsToCoords (g ^. balls) = if g ^. fireCountDown > 0 then FireBall_ else Ball
      | foldl (||) False (fmap (isFireBuff c) (g ^. buffs)) = FireBallBuff
      | foldl (||) False (fmap (isSplitBuff c) (g ^. buffs)) = SplitBuff
      | foldl (||) False (fmap (isSingleLifeBrick c) (g ^. pureBricks)) = PureBrick
      | foldl (||) False (fmap (isMultiLifeBrick c) (g ^. pureBricks)) = MultiLifeBrick
      | foldl (||) False (fmap (isBrick c . (^. brickCoord)) (g ^. hardBricks)) = HardBrick
      | c `elem` ballsToCoords (g ^. bullets) = Bullet
      | otherwise = Empty

-- util

isMultiLifeBrick :: V2 Int -> BrickState -> Bool
isMultiLifeBrick c b = isBrick c (b^.brickCoord) && (b^.isMultiLife)

isSingleLifeBrick :: V2 Int -> BrickState -> Bool
isSingleLifeBrick c b = isBrick c (b^.brickCoord) && (not $ b^.isMultiLife)

isFireBuff :: V2 Int -> BuffState  -> Bool
isFireBuff c b = (==) c (b^.buffCoord) && (b^.buffT == FireBall)

isSplitBuff :: V2 Int -> BuffState  -> Bool
isSplitBuff c b = (==) c (b^.buffCoord) && (b^.buffT == Split)

ballsToCoords :: Seq BallState -> Seq (V2 Int)
ballsToCoords = fmap f
  where
    f b = b ^. ballCoord

buffsToCoords :: Seq BuffState -> Seq (V2 Int)
buffsToCoords = fmap f
  where
    f b = b ^. buffCoord

--

drawTimeBar :: Game -> Widget Name
drawTimeBar g =
  overrideAttr progressCompleteAttr timeBarAttr $
    vLimit 3 $
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
drawCell MultiLifeBrick = withAttr mBricksAttr cw
drawCell HardBrick = withAttr hardBricksAttr cw
drawCell FireBallBuff = withAttr fireBallBuffAttr firebuffw
drawCell SplitBuff = withAttr splitBallBuffAttr splitbuffw

drawCell FireBall_ = withAttr fireBallAttr ballw
drawCell Bullet = withAttr bulletAttr bulletw

drawHelp :: Widget Name
drawHelp = hLimit 30
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str " Help ")
    $ vBox
    $ map
        (\a -> drawStat (sel1 a) (sel2 a))
        [ ("Move Left: ", "←"),
          ("Move Right: ", "→"),
          ("Machine Gun: ", "space"),
          ("Reset: ", "r"),
          ("Next Level: ", "g"),
          ("Pause/Resume: ", "p"),
          ("Quit: ", "q")
        ]

cw :: Widget Name
cw = str "  "

ballw :: Widget Name
ballw = str "⬤ "

firebuffw :: Widget Name
firebuffw = str "🔥"

splitbuffw :: Widget Name
splitbuffw = str "💖"

bulletw :: Widget Name
bulletw = str "💣"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (playerAttr, bg $ V.rgbColor 169 158 171),
      (noticeStringAttr, V.rgbColor 126 110 138 `on` bgColor `V.withStyle` V.bold),
      (pureBricksAttr, bg $ V.rgbColor 220 191 202),
      (mBricksAttr, bg $ V.rgbColor 208 138 137),
      (hardBricksAttr, V.yellow `on` V.yellow),
      (ballsAttr, V.rgbColor 96 87 110 `on` bgColor),
      (timeBarAttr, V.rgbColor 209 209 209 `on` V.rgbColor 113 151 195),
      (fireBallBuffAttr, bg $ V.rgbColor 251 111 80),
      (splitBallBuffAttr, bg $ V.rgbColor 251 111 80),
      (fireBallAttr, V.rgbColor 218 177 222 `on` bgColor),
      (bulletAttr, fg V.brightRed `V.withStyle` V.bold),
      (emptyAttr, bg bgColor),
      (globalAttr, bg bgColor)
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

mBricksAttr :: AttrName
mBricksAttr = "mBricks"

hardBricksAttr :: AttrName
hardBricksAttr = "hardBricks"

timeBarAttr :: AttrName
timeBarAttr = "timeBar"

fireBallBuffAttr :: AttrName
fireBallBuffAttr = "fireBallBuff"

splitBallBuffAttr = "splitBallBuff"

fireBallAttr :: AttrName
fireBallAttr = "fireBall"

bulletAttr :: AttrName
bulletAttr = "bullet"

globalAttr :: AttrName 
globalAttr = "global"