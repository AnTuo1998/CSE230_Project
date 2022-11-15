module Main where

import BB
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.Exit
import Test.HUnit
import UI.Game

testInitConfig = InitConfig { _initLevel = 0,
                    _initScore = 0,
                    _initHighestScore = 0,
                    _initPureBricks = S.fromList [],
                    _initHardBricks = S.fromList [],
                    _initTimeLimit = floor $ 240000000.0 / 55000,
                    _initTickInterval = 55000
                  }

testBallState = BallState
        { _ballCoord = V2 1 1,
          _hDir = East,
          _vDir = North
        }

-- testId = TestCase (assertEqual "Always Correct" False False)

testBuffs g = test [testActSplit1 g, testActSplit2 g]

testActSplit1 g = TestCase $ assertEqual "Normal Split" (length (g'^.balls)) (length testBalls + 1)
  where
    g' = actSplit $ g & balls .~ testBalls
    testBalls = S.fromList $ [testBallState]

testActSplit2 g = TestCase $ assertEqual "Never go beyond maxBalls" (g'^.balls) testBalls
  where
    g' = actSplit $ g & balls .~ testBalls
    testBalls = S.fromList $ replicate maxBalls testBallState

main :: IO ()
main = do
  putStrLn "\nTesting... "
  game <- initGame testInitConfig
  counts <-
    runTestTT
      ( test
          [ testBuffs game
          ]
      )
  putStrLn ("Tests Done")
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
