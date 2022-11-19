{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BB where

import Buff
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe ()
import Control.Monad.Trans.State ()
import Data.Foldable (toList)
import Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2), Sel3 (sel3))
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
  | Void
  deriving (Eq, Show)

data GameStatus
  = Ready
  | Paused
  | Win
  | Dead
  | Playing
  deriving (Eq, Show)

data BallState = BallState
  { _ballCoord :: Coord,
    _hDir :: Direction,
    _vDir :: Direction
  }
  deriving (Eq, Show)

makeLenses ''BallState

type BulletState = BallState

data BrickState = BrickState
  { _brickCoord :: Coord,
    _brickLife :: Int,
    _isMultiLife :: Bool
  }
  deriving (Eq, Show)

makeLenses ''BrickState

data InitConfig = InitConfig
  { _initLevel :: Int,
    _initScore :: Int,
    _initHighestScore :: Int,
    _initPureBricks :: Seq BrickState,
    _initHardBricks :: Seq BrickState,
    _initTimeLimit :: Int, -- in ticks
    _initTickInterval :: Int -- in 1e-6s
  }
  deriving (Eq, Show)

makeLenses ''InitConfig

data Game = Game
  { _initConfig :: InitConfig,
    -- | Player as a sequence of points in N2
    _player :: Player,
    -- | Game Status,
    _status :: GameStatus,
    -- | score
    _score :: Int,
    -- | life count
    _lifeCount :: Int,
    -- | total life count
    _totalLifeCount :: Int,
    -- | all bricks
    _pureBricks :: Seq BrickState,
    _hardBricks :: Seq BrickState,
    -- | all balls
    _balls :: Seq BallState,
    _buffs :: Seq BuffState,
    _hasMachineGun :: Bool,
    _numBullets :: Int,
    _bullets :: Seq BulletState,
    _timeLimit :: Int,
    _progress :: Int,
    _level :: Int,
    _highestScore :: Int,
    -- | the fireball buff
    _fireCountDown :: Int,
    _playNextLevel :: Bool
  }
  deriving (Eq, Show)

makeLenses ''Game

-------------------------------------------------------------------------------

-- | Constants

-------------------------------------------------------------------------------

height, width :: Int
height = 27
width = 30

playerLen = 6
playerSpeed = 3
fireCountInitVal = 100

brickLen = 2

reward = 10

-- max balls that can exist at the same time; 
-- split buff will be void if max balls are reached.
maxBalls = 3
scheduleFire = [20 + i * 150 | i <- [0 .. 20]]

scheduleSplit = [80 + i * 200 | i <- [0 .. 20]]

-------------------------------------------------------------------------------

-- | Game

-------------------------------------------------------------------------------

pause :: Game -> Game
pause g = if g ^. status == Playing then g & status .~ Paused else g

resume :: Game -> Game
resume g = if g ^. status `elem` [Paused, Ready] then g & status .~ Playing else g

step :: Game -> Game
step g = if g ^. status == Playing then stepHelper g else g

stepHelper :: Game -> Game
stepHelper = setGameWin . setGameOver . anotherChance . updateLifeCount . moveBuffs . actBuffs . clearBall . hitBricks . fireHit . hitBricksWithBullets . bounceWalls . runBalls . runBullets . advanceTime . expireBuff

advanceTime :: Game -> Game
advanceTime g = g & progress .~ g ^. progress + 1

expireBuff :: Game -> Game
expireBuff g = g & fireCountDown %~ subtract 1

setGameWin :: Game -> Game
setGameWin g = if null $ g ^. pureBricks then g & status .~ Win else g

setGameOver :: Game -> Game
setGameOver g = if g ^. timeLimit <= g ^. progress || g ^. lifeCount <= 0 then g & status .~ Dead else g

clearBall :: Game -> Game
clearBall g = g & balls .~ newBalls
  where
    newBalls = S.filter (\b -> b ^. ballCoord . _y >= 0) (g ^. balls)

-- Seperate this from anotherChance to guarantee lifeCount -1
-- before giving a new life (drawing a new ball) on the board.
updateLifeCount :: Game -> Game
updateLifeCount g = 
  if null (g ^. balls)
    then
      g & lifeCount %~ subtract 1
    else g

anotherChance :: Game -> Game
anotherChance g =
  if null (g ^. balls)
    then
      g & balls .~ newBalls
        & status .~ Paused
    else g
  where
    newBalls = if g ^. lifeCount > 0
      then initBalls (g ^. player . _x)
      else S.Empty

initGame :: InitConfig -> IO Game
initGame initConf =
  return
    Game
      { _initConfig = initConf,
        _player = V2 (width `div` 2) 0,
        _score = initConf ^. initScore,
        _status = Ready,
        _pureBricks = initConf ^. initPureBricks,
        --  [V2 1 16, V2 4 15, V2 7 15, V2 4 16, V2 7 16, V2 7 22, V2 10 22]
        _hardBricks = initConf ^. initHardBricks,
        -- [V2 1 15, V2 9 19]
        _balls = initBalls (div width 2),
        _buffs = initBuff,
        _hasMachineGun = True,
        _numBullets = 5,
        _bullets = S.Empty,
        -- _ballDirs = S.fromList [(East, North)],
        _timeLimit = initConf ^. initTimeLimit, -- in ticks
        _progress = 0,
        _level = initConf ^. initLevel,
        _highestScore = initConf ^. initHighestScore,
        _lifeCount = 3,
        _totalLifeCount = 3,
        _fireCountDown = 0,
        _playNextLevel = False
      }

-------------------------------------------------------------------------------

-- | Player

-------------------------------------------------------------------------------

bouncePlayer :: V2 Int -> BallState -> BallState
bouncePlayer player b =
  let y = b ^. ballCoord . _y
      d2 = b ^. vDir
   in if y == 1 && d2 == South && withinPlayer (b ^. ballCoord) player then oppositeBallVert b else b

-- TODO: x + playerLen <= or < width
isPlayerInBound :: Coord -> Bool
isPlayerInBound (V2 x _) = 0 <= x && x + playerLen <= width

withinPlayer :: V2 Int -> V2 Int -> Bool
withinPlayer (V2 bx _) (V2 px _) = bx >= px && bx <= px + playerLen

movePlayer :: Direction -> Game -> Game
movePlayer dir g =
  let newCoord = moveCoord playerSpeed dir (g ^. player)
      newG = resume g
   in if isPlayerInBound newCoord
        then newG & player .~ newCoord
        else newG

-------------------------------------------------------------------------------

-- | Bricks' bouncing behaviors

-------------------------------------------------------------------------------

-- ball_coord, brick_boord
withinHardBrick :: V2 Int -> V2 Int -> Bool
withinHardBrick (V2 bx by) (V2 hx hy) = bx >= hx && bx < hx + brickLen

moveCoord :: Int -> Direction -> Coord -> Coord
moveCoord n West (V2 x y) = V2 (x - n) y
moveCoord n East (V2 x y) = V2 (x + n) y
moveCoord n North (V2 x y) = V2 x (y + n)
moveCoord n South (V2 x y) = V2 x (y - n)
moveCoord n Void (V2 x y) = V2 x y

isBrick :: V2 Int -> V2 Int -> Bool
isBrick xy@(V2 x y) hb@(V2 hx hy) = y == hy && withinHardBrick xy hb

isHittingDiag :: BrickState -> BallState -> Maybe (BallState, BrickState)
isHittingDiag brick bst =
  if (by == hy - 1 || by == hy + 1)
    && (bx == hx - 1 || bx == hx + brickLen)
    && isBrick (nextPos (bst ^. hDir) (bst ^. vDir) (bst ^. ballCoord)) (brick^.brickCoord)
    then Just (oppositeBallVert bst, brick)
    else Nothing
  where
    by = bst ^. ballCoord . _y
    bx = bst ^. ballCoord . _x
    hx = brick ^. brickCoord ._x
    hy = brick ^. brickCoord ._y

isHittingVert :: BrickState -> BallState -> Maybe (BallState, BrickState)
isHittingVert brick bst =
  if ((by == hy - 1 && vD == North) || (by == hy + 1 && vD == South)) && withinHardBrick (bst ^. ballCoord) (brick^.brickCoord)
    then Just (oppositeBallVert bst, brick)
    else Nothing
  where
    by = bst ^. ballCoord . _y
    bx = bst ^. ballCoord . _x
    hy = brick ^. brickCoord ._y
    vD = bst ^. vDir

isHittingHori :: BrickState -> BallState -> Maybe (BallState, BrickState)
isHittingHori brick bst =
  if by == hy && ((bx == hx - 1 && hD == East) || (bx == hx + brickLen && hD == West))
    then Just (oppositeBallHori bst, brick)
    else Nothing
  where
    by = bst ^. ballCoord . _y
    bx = bst ^. ballCoord . _x
    hx = brick ^. brickCoord ._x
    hy = brick ^. brickCoord ._y
    hD = bst ^. hDir


isInside :: BrickState -> BallState -> Maybe (BallState, BrickState)
isInside brick bst =
  if isBrick (bst ^. ballCoord) (brick^.brickCoord)
    then Just (bst, brick)
    else Nothing
  where
    by = bst ^. ballCoord . _y
    bx = bst ^. ballCoord . _x

-- todo: change this to a 1-liner like foldl
batchCheck :: (BrickState -> BallState -> Maybe (BallState, BrickState)) -> Seq BrickState -> BallState -> Maybe (BallState, BrickState)
batchCheck f S.Empty _ = Nothing
batchCheck f (bricks :|> brick) ball =
  let ret = f brick ball
   in if isNothing ret
        then batchCheck f bricks ball
        else ret

bounceBricks :: Seq BrickState -> BallState -> Maybe (BallState, BrickState)
bounceBricks S.Empty bst = Nothing
bounceBricks bricks ball =
  batchCheck (\a b -> isHittingVert a b <|> isHittingHori a b) bricks ball
    <|> batchCheck isHittingDiag bricks ball

-- todo: merge this into bouncePureBricks
bounceHardBricks :: Seq BrickState -> BallState -> BallState
bounceHardBricks bricks ball =
  let ret = bounceBricks bricks ball
   in case ret of
        Nothing -> ball
        Just (ball', _) -> ball'

emptyBrick = BrickState {
  _brickCoord = V2 0 0,
  _brickLife = 0,
  _isMultiLife = False
}

bouncePureBricks :: Seq BrickState -> BallState -> (BallState, BrickState, Int)
bouncePureBricks bricks ball =
  let ret = bounceBricks bricks ball
   in case ret of
        Nothing -> (ball, emptyBrick, 0)
        Just (ball', brick) -> (ball', brick, reward)

nextPos :: Direction -> Direction -> Coord -> Coord
nextPos d1 d2 = moveCoord 1 d2 . moveCoord 1 d1

-------------------------------------------------------------------------------

-- | Fire balls' bouncing behaviors

-------------------------------------------------------------------------------

fireBounceBricks :: Seq BrickState -> BallState -> Maybe (BallState, BrickState)
fireBounceBricks S.Empty bst = Nothing
fireBounceBricks bricks ball = batchCheck isInside bricks ball

fireBouncePureBricks :: Seq BrickState -> BallState -> (BallState, BrickState, Int)
fireBouncePureBricks bricks ball =
  let ret = fireBounceBricks bricks ball
   in case ret of
        Nothing -> (ball, emptyBrick, 0)
        Just (ball', brick) -> (ball', brick, reward)

fireHit :: Game -> Game
fireHit g = g & score .~ newScore & pureBricks .~ newBricks
  where
    results = fmap (fireBouncePureBricks (g ^. pureBricks)) (g ^. balls)
    newScore = foldl (+) (g ^. score) (fmap sel3 results)
    bricksToDelete = fmap sel2 results
    newBricks = S.filter (`notElem` bricksToDelete) (g ^. pureBricks)

-------------------------------------------------------------------------------

-- | Balls

-------------------------------------------------------------------------------

isInBound :: Coord -> Bool
isInBound (V2 x y) = 0 <= x && x < width && 0 <= y

oppositeDir :: Direction -> Direction
oppositeDir East = West
oppositeDir West = East
oppositeDir South = North
oppositeDir North = South
oppositeDir Void = Void

oppositeBallHori :: BallState -> BallState
oppositeBallHori b = b & hDir %~ oppositeDir

oppositeBallVert :: BallState -> BallState
oppositeBallVert b = b & vDir %~ oppositeDir

bounceWallsHori :: BallState -> BallState
bounceWallsHori b =
  let x = b ^. ballCoord . _x
   in if (x <= 0 && b^.hDir == West) || (x >= width - 1 && b^.hDir == East) then oppositeBallHori b else b

bounceWallsVert :: BallState -> BallState
bounceWallsVert b =
  let y = b ^. ballCoord . _y
   in if (y >= height - 1) && b^.vDir == North then oppositeBallVert b else b

hitBricks :: Game -> Game
hitBricks g = g & balls .~ newBalls & score .~ newScore & pureBricks .~ filteredNewBricks & buffs %~ joinSeq newBuff
  where
    results = fmap (bouncePureBricks (g ^. pureBricks)) (g ^. balls)
    newBalls = if g ^. fireCountDown > 0 then g ^. balls else fmap sel1 results
    newScore = foldl (+) (g ^. score) (fmap sel3 results)
    bricksHit = S.filter (/= emptyBrick) (fmap sel2 results)
    newBuff = if null bricksToDelete then S.empty else dropBuff g (head $ toList bricksToDelete)
    newBricks = fmap (\b -> if b `elem` bricksHit then b&brickLife %~ (subtract 1) else b) (g ^. pureBricks)
    bricksToDelete = S.filter (\b -> b^.brickLife < 0) newBricks
    filteredNewBricks = S.filter (`notElem` bricksToDelete) newBricks

bounceWalls :: Game -> Game
bounceWalls g = g & balls .~ newBalls
  where
    changeDirs ball = bounceHardBricks (g ^. hardBricks) . bouncePlayer (g ^. player) . bounceWallsHori . bounceWallsVert $ ball
    newBalls = fmap changeDirs (g ^. balls)

runBalls :: Game -> Game
runBalls g = g & balls .~ newBalls
  where
    runBall b = b & ballCoord .~ nextPos (b ^. hDir) (b ^. vDir) (b ^. ballCoord)
    newBalls = fmap runBall (g ^. balls)

initBalls playerX =
  S.fromList
    [ BallState
        { _ballCoord = V2 playerCenter 1,
          _hDir = East,
          _vDir = North
        },
      BallState
        { _ballCoord = V2 playerCenter 1,
          _hDir = West,
          _vDir = North
        }
    ]
  where
    playerCenter = playerX + div playerLen 2

-------------------------------------------------------------------------------

-- | Buffs

-------------------------------------------------------------------------------

joinSeq :: Seq a -> Seq a -> Seq a
joinSeq x y = S.fromList $ toList x ++ toList y

-- following a fixed schedule (can be made random)
dropBuff :: Game -> BrickState -> Seq BuffState
dropBuff g brick = newBuff
  where
    newBuff = S.empty `joinSeq` newFireBall `joinSeq` newSplit
    newFireBall = if g ^. score `elem` scheduleFire then newBuffFac dropCoord FireBall else S.fromList []
    newSplit = if g ^. score `elem` scheduleSplit then newBuffFac dropCoord Split else S.fromList []
    dropCoord = moveCoord (div brickLen 2) East (brick^.brickCoord)

moveBuffs :: Game -> Game
moveBuffs g = if g ^. progress `mod` 3 == 0 then g & buffs .~ newBuffs else g
  where
    moveBuff b = b & buffCoord .~ moveCoord 1 South (b ^. buffCoord)
    newBuffs = fmap moveBuff (g ^. buffs)

actBuffs :: Game -> Game
actBuffs g = newG
  where
    actBuff bs g =
      if null bs
        then g
        else case head bs ^. buffT of
          FireBall -> actFireBall g
          Split -> actSplit g
    liveBuffs = S.filter (\b -> b ^. buffCoord . _y > 0) (g ^. buffs)
    -- catchedBuff should be no more than 1 element
    catchedBuff = toList $ S.filter (\b -> b ^. buffCoord . _y == 0 && withinPlayer (b ^. buffCoord) (g ^. player)) (g ^. buffs)
    newG = actBuff catchedBuff (g & buffs .~ liveBuffs)

actFireBall :: Game -> Game
actFireBall g = g & fireCountDown .~ fireCountInitVal

actSplit :: Game -> Game
actSplit g = if null (g ^. balls) || length (g^.balls) >= maxBalls then g else g & balls %~ joinSeq newBall
  where
    newBall = S.fromList [oppositeBallVert ballToSplit]
    ballToSplit = head $ toList $ g ^. balls

-------------------------------------------------------------------------------

-- | Machine Gun

-------------------------------------------------------------------------------

machineGun :: Game -> Game
machineGun g = if g ^. numBullets > 0
  then
    g & bullets %~ joinSeq newBullet & numBullets %~ subtract 1
  else g
    where
        newBullet =
          S.fromList
            [ BallState
                { _ballCoord = V2 playerCenter 1,
                  _hDir = Void,
                  _vDir = North
                }
            ]
        playerCenter = g ^. player . _x + div playerLen 2

runBullets :: Game -> Game
runBullets g = g & bullets .~ newBullets
  where
    runBullet b = b & ballCoord .~ nextPos (b ^. hDir) (b ^. vDir) (b ^. ballCoord)
    newBullets = fmap runBullet (g ^. bullets)

bounceBricksWithBullets :: Seq BrickState -> BulletState -> Maybe (BulletState, BrickState)
bounceBricksWithBullets S.Empty bst = Nothing
bounceBricksWithBullets bricks bullet =
  batchCheck isHittingVert bricks bullet

bouncePureBricksWithBullets :: Seq BrickState -> BulletState -> (BulletState, BrickState, Int)
bouncePureBricksWithBullets bricks bullet =
  let ret = bounceBricksWithBullets bricks bullet
   in case ret of
        Nothing -> (bullet, emptyBrick, 0)
        Just (bullet', brick) -> (bullet', brick, reward)

hitBricksWithBullets :: Game -> Game
hitBricksWithBullets g = g & bullets .~ newBullets & score .~ newScore & pureBricks .~ filteredNewBricks & buffs %~ joinSeq newBuff
  where
    results = fmap (bouncePureBricksWithBullets (g ^. pureBricks)) (g ^. bullets)
    newBullets = fmap sel1 (S.filter (\x -> sel3 x == 0) results)
    newScore = foldl (+) (g ^. score) (fmap sel3 results)
    bricksHit = S.filter (/= emptyBrick) (fmap sel2 results)
    newBuff = if null bricksToDelete then S.empty else dropBuff g (head $ toList bricksToDelete)
    newBricks = fmap (\b -> if b `elem` bricksHit then b & brickLife %~ subtract 1 else b) (g ^. pureBricks)
    bricksToDelete = S.filter (\b -> b ^. brickLife < 0) newBricks
    filteredNewBricks = S.filter (`notElem` bricksToDelete) newBricks
