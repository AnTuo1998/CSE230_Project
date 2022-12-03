module UI.Theme (bgColor, defAttr) 
where
import Brick
  ( bg,
    on
  )
import qualified Graphics.Vty as V

bgColor = V.rgbColor 224 226 226
fgColor = V.black
defAttr =  fgColor `on` bgColor
