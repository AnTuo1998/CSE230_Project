module Main where

import BB
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode) )
import UI.Game (playGame)
import UI.Home (startHome)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..))
import Data.List(intercalate)
import Data.List.Split(splitOn)
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2))
import Control.Lens ((&), (.~), element)

main :: IO ()
main = do
  level' <- startHome
  prevScores <- readScores
  map' <- readMap
  tickInterval <- readInterval level'
  g <- playGame (let intervalFloat = fromIntegral tickInterval :: Float
                    in InitConfig 
                    { _initLevel=level',
                      _initHighestScore=prevScores !! level',
                      _initPureBricks=sel1 map',
                      _initHardBricks=sel2 map',
                      _initTimeLimit=floor $ 240000000.0/intervalFloat,
                      _initTickInterval=tickInterval
                    })
  saveResults (_score g) (_level g)

readMap :: IO (Seq BrickLoc, Seq BrickLoc)
readMap = do
            raw <- readFile "src/maps/map.txt"
            return $ parseMap raw

parseMap :: String -> (Seq BrickLoc, Seq BrickLoc)
parseMap s = (S.fromList a, S.fromList b)
  where
    tokens = zip2d . splitOn "\n" $ s  -- [(i,j),x]
    (a, b) = parseHelper tokens


parseHelper :: [((Int,Int), Char)] -> ([V2 Int], [V2 Int])
parseHelper [] = ([], [])
parseHelper tokens@(((i,j), x):ts) = case x of
                                    '#' -> let (a,b) = parseHelper $ drop 2 tokens
                                                 in (a ++ [V2 i (height -j)], b)
                                    '=' -> let (a,b) = parseHelper $ drop 2 tokens
                                                 in (a, b ++ [V2 i (height - j)])
                                    _ -> parseHelper ts


-- return (S.fromList[V2 1 16, V2 4 15, V2 7 15, V2 4 16, V2 7 16, V2 7 22, V2 10 22],S.fromList [V2 1 15, V2 9 19])

zip2d :: [[a]] -> [((Int,Int), a)]
zip2d ll = [((i,j), x) | (j, l) <- zip [0..] ll,
                         (i, x) <- zip [0..] l
                                 ]
scoreFilePath :: String
scoreFilePath = "src/highest.txt"

intervalFilePath :: String
intervalFilePath = "src/interval.txt"

saveResults :: Int -> Int -> IO ()
saveResults score' level' = do
  prevScores <- readScores
  let prevScore = prevScores !! level'
    in if prevScore >= score' 
      then return ()
      else saveScores $ intercalate " " (map show (prevScores & element level' .~ score'))

saveScores :: String -> IO ()
saveScores s = do
  scoreFile <- openFile scoreFilePath WriteMode
  hPutStrLn scoreFile s
  hClose scoreFile

parseNumbers :: String -> [Int]
parseNumbers s = fmap (read :: String -> Int) (words s)

readScores :: IO [Int]
readScores = do
  contents <- readFile scoreFilePath
  putStr contents -- this line is needed! otherwise read/write won't work due to lazyIO 
  return $ parseNumbers contents

readInterval :: Int -> IO Int 
readInterval level' = do
  contents <- readFile intervalFilePath
  return$ (parseNumbers contents) !! level'
  -- let result = fromIntegral $ (parseNumbers contents) !! level :: Float
  --   in return result
