module UI.Level
  ( chooseLevel
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)

app :: App (Maybe Int) e ()
app =
  App
    { appDraw = const [ui],
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

ui :: Widget ()
ui =
  padLeft (Pad 21) $
    padRight (Pad 23) $
      C.center $ vLimit 19 $ hLimit 70 $ withBorderStyle BS.unicodeBold $
        B.borderWithLabel (str "CSE230 Presents") $ C.center $
        C.center $
            vBox
            [ C.hCenter $
                vBox
                    [ str "Please enter level"
                    ],
                padTop (Pad 5) $ C.hCenter $ str "Choose Level 0-5"
            ]

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. '9']
    then halt $ Just (read [d])
    else continue n
handleEvent n _ = continue n

chooseLevel :: IO Int
chooseLevel = defaultMain app Nothing >>= maybe exitSuccess return