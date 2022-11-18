module UI.Help
  ( showHelp
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( viewport
  )
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
  padLeft (Pad 21) $ padRight (Pad 23) $ C.center $ vLimit 22 $ hLimit 70 $ withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str " CSE230 Presents ") $ C.center $
    C.center $
        vBox
        [ C.hCenter $
            vBox
                [ padLeftRight 2 $ padTop (Pad 1) $ drawHelpDoc,
                  padLeftRight 1 $ padTop (Pad 2) $ str "Scroll with ↑/↓ and press q to return"
                ]
        ]

handleEvent :: () -> BrickEvent () e -> EventM () (Next ())
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey V.KDown _)) = vScrollBy (viewportScroll ()) 1 >> continue n
handleEvent n (VtyEvent (V.EvKey V.KUp _)) = vScrollBy (viewportScroll ()) (-1) >> continue n
handleEvent n _ = continue n

showHelp :: IO ()
showHelp = defaultMain app ()

data Name = ViewPointName
  deriving (Show,Eq,Ord)

drawHelpDoc :: Widget ()
drawHelpDoc = viewport () Vertical $
  C.hCenter $ strWrap helpStr

helpStr :: String 
helpStr = "Welcome to Brick Breaker game!\n\n" ++ 
        "This game is based on the Brick library in Haskell. The player should smash a wall of bricks by deflecting a bouncing ball with a paddle. " ++
        "The paddle is controlled by keyboard and moves horizontally.\n\n" ++ 
        "Description:\n\n" ++
        "Paddle\n" ++
        "Control the paddle with ←/→ and bounce the balls back as they are falling.\n" ++
        "Balls:\n" ++
        "Constantly bounce and hit the wall bricks.\n" ++
        "Buff:\n" ++
        "There are randomly generated buffs when the ball smashs a brick to help complete the level. Catch the dropping buff with the paddle. There are two kind of buffs in the game.\n" ++
        " - Fireball: the balls will be powered up and hit through the bricks instead of bouncing back for a period of time.\n" ++
        " - Split: each ball will split into two balls going to different directions.\n" ++
        "Machine gun:\n" ++
        "Additional help to destroy the bricks. Fire the machine gun with five bullets in each level and every bullet is equivalent to a hit on the brick.\n\n" ++

        "Available interactions:\n\n" ++
        "←/→            Move the paddle to left/right\n" ++ 
        "space          Machine gun\n" ++ 
        "r              Restart the current level\n" ++
        "g              Go into the next available level\n" ++
        "q              Quit/Return to the homepage\n" ++ 
        "p              Pause/Resume the game\n"