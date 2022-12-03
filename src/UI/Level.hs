{-# LANGUAGE TemplateHaskell #-}
module UI.Level
  ( chooseLevel,
    getLevel,
    getUsername
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)
import Brick.Forms ( (@@=), Form (formState), newForm, editTextField, editShowableField, renderForm, invalidFormInputAttr, focusedFormInputAttr, handleFormEvent, setFieldValid, setFormFocus )
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified Data.Text as T
import qualified Brick.Widgets.Edit as E
import UI.Theme (defAttr)

data ResourceName
  = NameField
  | LevelField
  deriving (Eq, Ord, Show)

-- form
data LevelState =
    LevelState { _name      :: T.Text
              , _level       :: Int
              } deriving (Show)

makeLenses ''LevelState

mkForm :: LevelState -> Form LevelState e ResourceName
mkForm =
    let label s w = padRight (Pad 10) $ padLeft (Pad 10) $ padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Username" @@=
                  editTextField name NameField (Just 1)
               , label "Level" @@=
                  editShowableField level LevelField
               ]

-- utilities
getLevel :: Form LevelState e ResourceName -> Int
getLevel s =
  let state = formState s in
  _level state

getUsername :: Form LevelState e ResourceName -> String
getUsername s =
  let state = formState s in
  T.unpack $ _name state

-- app
app :: App (Form LevelState e ResourceName) e ResourceName
app =
  App
    { appDraw = drawLevel,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap defAttr [(E.editAttr, fg V.black),
      (E.editFocusedAttr, V.black `on` V.yellow), (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow) ],
      appChooseCursor = neverShowCursor
    }


drawLevel :: Form LevelState e ResourceName -> [Widget ResourceName]
drawLevel st =
  [padLeft (Pad 21) $
    padRight (Pad 23) $
      C.center $ vLimit 19 $ hLimit 70 $ withBorderStyle BS.unicodeBold $
        B.borderWithLabel (str " CSE230 Presents ") $ C.center $
        C.center $
            vBox
            [ C.vCenter $ C.hCenter $ renderForm st, 
            padTop (Pad 1) $ C.hCenter $ str "Username cannot be empty",  
            padBottom (Pad 3) $ C.hCenter $ str "Level can only be chosen from 0 - 5"]
           ]

handleEvent :: Form LevelState e ResourceName -> BrickEvent ResourceName e -> EventM ResourceName (Next (Form LevelState e ResourceName))
handleEvent s e =
  case e of
    VtyEvent (V.EvKey (V.KChar 'q') _) -> halt $ mkForm LevelState {_name = T.pack "", _level = -1}
    VtyEvent (V.EvKey V.KEnter []) -> if let l = getLevel s; n = getUsername s in l >= 0 && l <= 5 && n /= "" then halt s else continue s
    VtyEvent (V.EvKey V.KDown []) -> 
      continue $ setFormFocus LevelField s
    VtyEvent (V.EvKey V.KUp []) -> 
      continue $ setFormFocus NameField s
    _ -> do
      s' <- handleFormEvent e s
      continue $ setFieldValid (let l = getLevel s' in l >= 0 && l <= 5 ) LevelField $ setFieldValid (let n = getUsername s' in  n /= "" ) NameField s'

chooseLevel :: IO (Form LevelState e ResourceName)
chooseLevel = do
  defaultMain app (mkForm LevelState {_name = T.pack "Alice", _level = 0})