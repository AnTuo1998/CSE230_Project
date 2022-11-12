module UI.Help
  ( showHelp
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)

app :: App () e ()
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
  padLeft (Pad 21) $ padRight (Pad 23) $ C.center $ vLimit 19 $ hLimit 70 $ withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "CSE230 Presents") $ C.center $
    C.center $
        vBox
        [ C.hCenter $
            vBox
                [ str "TBD"
                ]
        ]

handleEvent :: () -> BrickEvent () e -> EventM () (Next ())
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n _ = continue n

showHelp :: IO ()
showHelp = defaultMain app ()