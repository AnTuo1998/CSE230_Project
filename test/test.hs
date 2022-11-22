module Main where

import BB
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.Exit
import Test.HUnit
import UI.Game

testInitConfig =
  InitConfig
    { _initLevel = 0,
      _initScore = 0,
      _initHighestScore = 0,
      _initPureBricks = S.fromList [],
      _initHardBricks = S.fromList [],
      _initTimeLimit = floor $ 240000000.0 / 55000,
      _initTickInterval = 55000
    }

testBallState =
  BallState
    { _ballCoord = V2 1 1,
      _hDir = East,
      _vDir = North
    }

testBrickState = emptyBrick

-- test Buffs

testBuffs g = test [testActSplit1 g, testActSplit2 g]

testActSplit1 g = TestCase $ assertEqual "Normal Split" (length (g' ^. balls)) (length testBalls + 1)
  where
    g' = actSplit $ g & balls .~ testBalls
    testBalls = S.fromList $ [testBallState]

testActSplit2 g = TestCase $ assertEqual "Never go beyond maxBalls" (g' ^. balls) testBalls
  where
    g' = actSplit $ g & balls .~ testBalls
    testBalls = S.fromList $ replicate maxBalls testBallState

-- test hitting Bricks

testHitBrick g = test [testHitVert g, testHitHori g]

testHitVert _ = TestCase $ assertEqual "Hit vertical" (isHittingVert brickSt ballSt) (Just (oppositeBallVert ballSt, brickSt))
  where
    brickSt = testBrickState & brickCoord .~ (V2 4 3)
    ballSt = testBallState & ballCoord .~ (V2 4 4) & vDir .~ South

testHitHori _ = TestCase $ assertEqual "Hit horizontal" (isHittingHori brickSt ballSt) (Just (oppositeBallHori ballSt, brickSt))
  where
    brickSt = testBrickState & brickCoord .~ (V2 4 3)
    ballSt = testBallState & ballCoord .~ (V2 3 3) & vDir .~ East

testBounceBricks _ = TestCase $ assertEqual "Bounce bricks" (bounceBricks brickStSeq ballSt) (Just (oppositeBallVert ballSt, brick1))
  where
    brickStSeq = S.fromList [brick1, brick2]
    brick1 = testBrickState & brickCoord .~ (V2 3 4)
    brick2 = testBrickState & brickCoord .~ (V2 10 6)
    ballSt = testBallState & ballCoord .~ (V2 3 3) & vDir .~ North

-- test hitting Walls

testWalls g = test [testBounceWalls g]

testBounceWalls g = TestCase $ assertEqual "Bounce walls & move" (bounceWalls inputG) expectedG
  where
    inputG = g & balls .~ S.fromList [testBallState & hDir .~ West & ballCoord .~ coord]
    expectedG = g & balls .~ S.fromList [testBallState & hDir .~ East & ballCoord .~ coord]
    coord = V2 0 5

-- test player

testPlayer g = test [testMovePlayer g]

testMovePlayer g = TestCase $ assertEqual "Move Player" (movePlayer dir g) expectedG
  where
    dir = East
    expectedG =
      g & player %~ (moveCoord playerSpeed dir)
        & status .~ Playing

main :: IO ()
main = do
  putStrLn "\nTesting... "
  game <- initGame testInitConfig
  counts <-
    runTestTT
      ( test
          [ testBuffs game,
            testHitBrick game,
            testPlayer game,
            testWalls game
          ]
      )
  putStrLn ("Tests Done")
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
