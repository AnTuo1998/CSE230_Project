module UI.Ranking
  ( showRanking
  )
where

import Brick hiding (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)
import Graphics.Vty
import Data.List (sortBy)
import Data.Function (on)

-- datatypes
type ResourceName = String
data RankingState = RankingState {
  record :: [(String, Int)]
}

buildInitState :: IO RankingState
buildInitState = do
  records <- readFile "src/record.txt"
  return RankingState {record = take 10 $ sortBy (flip compare `on` snd) $ concatMap createPair (lines records)}
  where
    createPair :: String -> [(String, Int)]
    createPair "" = []
    createPair s =
      let
        p = words s
      in
      case p of
        [name, score] -> [(name, read score :: Int)]
        _ -> []

app :: App RankingState e ResourceName
app =
  App
    { appDraw = drawRanking,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr [(attrName "top", fg V.yellow `withStyle` V.bold)],
      appChooseCursor = neverShowCursor
    }

drawRanking :: RankingState -> [Widget ResourceName]
drawRanking st =
  [padLeft (Pad 21) $ padRight (Pad 23) $ C.center $ vLimit 22 $ hLimit 70 $ withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "CSE230 Presents") $ C.center $
    vBox [ C.hCenter $
            vBox[
            padTop (Pad 1) $ C.hCenter $ str "Top 10",
            padTop (Pad 1) $ C.hCenter $ vBox $ map drawEntry $ record st,
            padTop (Pad 3) $ C.hCenter $ str "Press q to return"]
        ]]

drawEntry :: (String, Int) -> Widget ResourceName
drawEntry (name, score) = hBox [withAttr (attrName "top") (str name), padLeft (Pad 2) $ str $ show score]

handleEvent :: RankingState -> BrickEvent ResourceName e -> EventM ResourceName (Next RankingState)
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n _ = continue n

showRanking :: IO RankingState
showRanking = do
    initState <- buildInitState
    defaultMain app initState
    