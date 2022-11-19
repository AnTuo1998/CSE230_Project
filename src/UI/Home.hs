module UI.Home
  ( startHomeIni, startHomeRep, getPage, getCursorState, MenuCursor
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)
import System.IO
import Graphics.Vty.Attributes (withStyle)

-- data types
type ResourceName = String
data HomeState = HomeState {
  page :: Int, -- 0: default, 1: start, 2: ranking, 3: help
  menu :: MenuCursor String
}

data MenuCursor a = MenuCursor {
  prev :: [a],
  cur :: a,
  next :: [a]
}

-- make cursor
makeMenuCursor :: [a] -> MenuCursor a
makeMenuCursor (x:xs) = MenuCursor {prev = [], cur = x, next = xs}
makeMenuCursor []= error "menu is not empty, shouldn't reach here"

selectNext :: MenuCursor a -> MenuCursor a
selectNext cs = case next cs of
  [] -> cs
  x:xs -> MenuCursor {prev = curX:prevX, cur = x, next = xs}
  where
    curX = cur cs
    prevX = prev cs


selectPrev :: MenuCursor a -> MenuCursor a
selectPrev cs = case prev cs of
  [] -> cs
  x:xs -> MenuCursor {prev = xs, cur = x, next = curX:nextX}
  where
    curX = cur cs
    nextX = next cs

getPage :: HomeState -> Int
getPage = page

getCursorState :: HomeState -> MenuCursor String
getCursorState = menu

-- app
app :: App HomeState e ResourceName
app =
  App
    { appDraw = drawHome,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr [(attrName "selected", fg V.yellow `withStyle` V.bold)],
      appChooseCursor = neverShowCursor
    }

drawHome :: HomeState -> [Widget ResourceName]
drawHome st =
  [padLeft (Pad 21) $ padRight (Pad 23) $ C.center $ vLimit 22 $ hLimit 70 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "CSE230 Presents") $ C.center $
      vBox [ C.hCenter $
                vBox
                  [ C.hCenter $ str " ______  ______  __  ______  __  __\n/\\  == \\/\\  == \\/\\ \\/\\  ___\\/\\ \\/ /\n\\ \\  __<\\ \\  __<\\ \\ \\ \\ \\___\\ \\  _\"-.\n \\ \\_____\\ \\_\\ \\_\\ \\_\\ \\_____\\ \\_\\ \\_\\\n  \\/_____/\\/_/ /_/\\/_/\\/_____/\\/_/\\/_/",
                  C.hCenter $ str " ______  ______  ______  ______  __  __  ______  ______\n/\\  == \\/\\  == \\/\\  ___\\/\\  __ \\/\\ \\/ / /\\  ___\\/\\  == \\\n\\ \\  __<\\ \\  __<\\ \\  __\\\\ \\  __ \\ \\  _\"-\\ \\  __\\\\ \\  __<\n \\ \\_____\\ \\_\\ \\_\\ \\_____\\ \\_\\ \\_\\ \\_\\ \\_\\ \\_____\\ \\_\\ \\_\\\n  \\/_____/\\/_/ /_/\\/_____/\\/_/\\/_/\\/_/\\/_/\\/_____/\\/_/ /_/"],
                padTop (Pad 1) $ C.hCenter $ str "Welcome :)",
                padTop (Pad 1) $ C.hCenter $ vBox $ concat [map (drawEntry False) $ reverse $ prev m,
                      [drawEntry True (cur m)],
                      map (drawEntry False) $ next m],
                padTop (Pad 1) $ C.hCenter $ str "Press enter to select"]
            ]
  where
    m = menu st

drawEntry :: Bool -> String -> Widget n
drawEntry b s = if b then withAttr (attrName "selected") (str s) else str s

handleEvent :: HomeState -> BrickEvent ResourceName e -> EventM ResourceName (Next HomeState)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        V.EvKey V.KDown _ ->
          continue $ s {menu = selectNext (menu s)}
        V.EvKey V.KUp _ ->
          continue $ s {menu = selectPrev (menu s)}
        V.EvKey V.KEnter _ -> case selected of
          "Start" -> halt s {page = 1}
          "Ranking" -> halt s {page = 2}
          "Help" -> halt s {page = 3}
          "Quit" -> halt s
          _ -> error "should not reach here"
          where
            selected = cur $ menu s
        V.EvKey V.KEsc _ -> halt s
        _ -> continue s
    _ -> continue s

startHomeIni :: IO HomeState
startHomeIni = do
  finalState <- defaultMain app HomeState {page = 0, menu = makeMenuCursor ["Start", "Help", "Ranking", "Quit"]}
  if page finalState == 0 then exitSuccess else return finalState

startHomeRep :: MenuCursor String -> IO HomeState
startHomeRep cursor = do
  finalState <- defaultMain app HomeState {page = 0, menu = cursor}
  if page finalState == 0 then exitSuccess else return finalState