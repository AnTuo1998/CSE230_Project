{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module BB
  ( initGame,
    pause,
    step,
    InitConfig(..),
    Game (..),
    Direction (..),
    GameStatus (..),
    BallState,
    ballCoord,
    BrickLoc,
    initConfig,
    initLevel,
    initTickInterval,
    initTimeLimit,
    status,
    lifeCount,
    score,
    player,
    height,
    width,
    movePlayer,
    timeLimit,
    progress,
    pureBricks,
    hardBricks,
    balls,
    level,
    highestScore,
    moveCoord,
    isInBound,
    bounceWalls,
    withinPlayer,
    withinHardBrick,
    isHardBrick,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2), Sel3 (sel3))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.IO

-- Types

type Coord = V2 Int

type Player = Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data GameStatus
  = Paused
  | Win
  | Dead
  | Playing
  deriving (Eq, Show)

data BallState = BallState
  {
    _ballCoord :: Coord,
    _hDir :: Direction,
    _vDir :: Direction,
    _fireCountDown :: Int
  } 
  deriving (Show)

makeLenses ''BallState

type BrickLoc = Coord

data InitConfig = InitConfig 
  {
    _initLevel :: Int,
    _initHighestScore :: Int,
    _initPureBricks :: Seq BrickLoc,
    _initHardBricks :: Seq BrickLoc,
    _initTimeLimit :: Int, -- in ticks
    _initTickInterval :: Int -- in 1e-6s
  }
  deriving (Show)

makeLenses ''InitConfig
data Game = Game
  { 
    _initConfig :: InitConfig,
    -- | Player as a sequence of points in N2
    _player :: Player,
    -- | Game Status,
    _status :: GameStatus,
    -- | score
    _score :: Int,
    -- | life count
    _lifeCount :: Int,
    -- | all bricks
    _pureBricks :: Seq BrickLoc,
    _hardBricks :: Seq BrickLoc,
    -- | all balls 
    _balls :: Seq BallState,
    _timeLimit :: Int,
    _progress :: Int,
    _level :: Int,
    _highestScore :: Int
  }
  deriving (Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 27
width = 30

playerLen = 6
brickLen = 2

reward = 10
-- Functions

pause :: Game -> Game
pause g = if g^.status == Playing then g & status .~ Paused else g

resume :: Game -> Game
resume g = if g^.status == Paused then g & status .~ Playing else g

step :: Game -> Game
step g = if g^.status == Playing then stepHelper g else g

stepHelper:: Game -> Game
stepHelper = anotherChance . dropBall . hitBricks . bounceWalls . setGameWin. setGameOver . advanceTime

advanceTime :: Game -> Game
advanceTime g = g & progress .~ ((g ^. progress) + 1)

setGameWin :: Game -> Game
setGameWin g = if null $ g^.pureBricks then g & status .~ Win else g

setGameOver :: Game -> Game
setGameOver g = if (g ^. timeLimit) <= (g ^. progress) || g^.lifeCount < 0 then g & status .~ Dead else g

oppositeDir :: Direction -> Direction
oppositeDir East = West
oppositeDir West = East
oppositeDir South = North
oppositeDir North = South

oppositeBallHori :: BallState -> BallState
oppositeBallHori b = b & hDir %~ oppositeDir
oppositeBallVert :: BallState -> BallState
oppositeBallVert b = b & vDir %~ oppositeDir

bounceHori :: BallState -> BallState
bounceHori b = let x = b ^.ballCoord._x
   in if x < 1 || x >= (width - 1) then oppositeBallHori b else b

bounceVert :: BallState -> BallState
bounceVert b = let y = b ^.ballCoord._y
   in if y >= (height - 1) then oppositeBallVert b else b

bouncePlayer :: V2 Int -> BallState -> BallState
bouncePlayer player b = let y = b ^.ballCoord._y
                            d2 = b^.vDir
   in if y == 1 && d2 == South && withinPlayer (b^.ballCoord) player then oppositeBallVert b else b
   
withinPlayer :: V2 Int -> V2 Int -> Bool
withinPlayer (V2 bx _) (V2 px _) = bx >= px && bx <= (px + playerLen)

-- ball_coord, brick_boord
withinHardBrick :: V2 Int -> V2 Int -> Bool
withinHardBrick (V2 bx by) (V2 hx hy) = bx >= hx && bx <= (hx + brickLen)

isInBound :: Coord -> Bool
isInBound (V2 x y) = 0 <= x && x < width && 0 <= y

moveCoord :: Int -> Direction -> Coord -> Coord
moveCoord n West (V2 x y) = V2 (x - n) y
moveCoord n East (V2 x y) = V2 (x + n) y
moveCoord n North (V2 x y) = V2 x (y + n)
moveCoord n South (V2 x y) = V2 x (y - n)

movePlayer :: Direction -> Game -> Game
movePlayer dir g =
  let newCoord = moveCoord 3 dir (g ^. player)
      newG = resume g
  in if isInBound newCoord
    then newG & player .~ newCoord
    else newG

isHardBrick xy@(V2 x y) hb@(V2 hx hy) = y == hy && withinHardBrick xy hb

dropBall :: Game -> Game
dropBall g = g & balls .~ newBalls 
  where newBalls = S.filter (\b -> b^.ballCoord._y >= 0) (g^.balls)

anotherChance :: Game -> Game
anotherChance g = if null $ g^.balls then g & lifeCount %~ subtract 1 
                                            & balls .~ newBalls
                                            & status .~ Paused
                                            else g
  where 
   
    newBalls = initBalls (g^.player._x)

isHittingDiag :: BrickLoc -> BallState -> Maybe (BallState, BrickLoc)
isHittingDiag brick@(V2 hx hy) bst = if (by == hy - 1 || by == hy + 1) &&
                                                                (bx == hx - 1 || bx == hx + 1 + brickLen)
                                                                && isHardBrick (nextPos (bst^.hDir) (bst^.vDir) (bst^.ballCoord)) brick
                                                         then Just (oppositeBallVert bst, brick)
                                                         else Nothing
  where by = bst^.ballCoord._y
        bx = bst^.ballCoord._x


isHittingVert :: BrickLoc -> BallState -> Maybe (BallState, BrickLoc)
isHittingVert brick@(V2 _ hy) bst = if (by == hy - 1 || by == hy + 1) && withinHardBrick (bst^.ballCoord) brick
                                                         then Just (oppositeBallVert bst, brick)
                                                         else Nothing
  where by = bst^.ballCoord._y
        bx = bst^.ballCoord._x

isHittingHori :: BrickLoc -> BallState -> Maybe (BallState, BrickLoc)
isHittingHori brick@(V2 hx hy) bst = if by == hy && (bx == hx - 1 || bx == hx + 1 + brickLen)
                                                         then Just (oppositeBallHori bst, brick)
                                                         else Nothing
  where by = bst^.ballCoord._y
        bx = bst^.ballCoord._x

-- todo: change this to a 1-liner like foldl
batchCheck :: (BrickLoc -> BallState -> Maybe (BallState, BrickLoc)) -> Seq BrickLoc -> BallState -> Maybe (BallState, BrickLoc)
batchCheck f S.Empty _ = Nothing
batchCheck f (bricks :|> brick) ball = let ret = f brick ball
                                       in if isNothing ret
                                       then batchCheck f bricks ball
                                       else ret

bounceBricks :: Seq Coord -> BallState -> Maybe (BallState, BrickLoc)
bounceBricks S.Empty bst = Nothing
bounceBricks bricks ball = batchCheck (\a b -> isHittingVert a b <|> isHittingHori a b) bricks ball
                           <|>
                           batchCheck isHittingDiag bricks ball

-- todo: merge this into bouncePureBricks
bounceHardBricks :: Seq BrickLoc -> BallState -> BallState
bounceHardBricks bricks ball = let ret = bounceBricks bricks ball
                                 in case ret of
                                 Nothing -> ball
                                 Just (ball', _) -> ball'

bouncePureBricks :: Seq BrickLoc -> BallState -> (BallState, BrickLoc, Int)
bouncePureBricks bricks ball = let ret = bounceBricks bricks ball
                                 in case ret of
                                 Nothing -> (ball, V2 0 0, 0)
                                 Just (ball', brick) -> (ball', brick, reward)

nextPos d1 d2 = moveCoord 1 d2. moveCoord 1 d1

hitBricks :: Game -> Game
hitBricks g = g & balls .~ newBalls & score .~ newScore & pureBricks .~ newBricks
  where
    results = fmap (bouncePureBricks (g^.pureBricks)) (g ^. balls)
    -- newBalls = fmap sel1 results
    newBalls = g^.balls
    newScore = foldl (+) (g^.score) (fmap sel3 results)
    bricksToDelete = fmap sel2 results
    newBricks = S.filter (`notElem` bricksToDelete) (g^.pureBricks)

bounceWalls :: Game -> Game
bounceWalls g = g & balls .~ newBalls
  where
    changeDirs ball = bounceHardBricks (g ^. hardBricks). bouncePlayer (g ^. player) . bounceHori . bounceVert $ ball
    runBall b = b & ballCoord .~ nextPos (b^.hDir) (b^.vDir) (b^.ballCoord)
    newBalls = fmap (runBall . changeDirs) (g ^. balls)

initGame :: InitConfig -> IO Game
initGame initConf =
  return Game
          { _initConfig = initConf,
            _player = V2 (width `div` 2) 0,
            _score = 0,
            _status = Paused,
            _pureBricks = initConf ^. initPureBricks,
            --  [V2 1 16, V2 4 15, V2 7 15, V2 4 16, V2 7 16, V2 7 22, V2 10 22]
            _hardBricks = initConf ^. initHardBricks,
            -- [V2 1 15, V2 9 19]
            _balls = initBalls (div width 2),
            -- _ballDirs = S.fromList [(East, North)],
            _timeLimit = initConf^.initTimeLimit, -- in ticks
            _progress = 0,
            _level = initConf^.initLevel,
            _highestScore = initConf^.initHighestScore,
            _lifeCount = 2
          }

initBalls playerX = S.fromList [BallState{_ballCoord =V2 playerX 1, 
                                    _hDir=East,
                                    _vDir=North,
                                    _fireCountDown=0}, BallState{_ballCoord =V2 playerX 1, 
                                    _hDir=West,
                                    _vDir=North,
                                    _fireCountDown=0}]