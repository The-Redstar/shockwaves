
module WaveForms.Color (
    Color(..),
    red,yellow,green,cyan,blue,magenta,white,gray,
) where

import Clash.Prelude
import Data.Word (Word8)


-- | Simple RGB representation used for custom signal colors.
data Color = RGB Word8 Word8 Word8 deriving Show

-- | #ff0000
red::Color
red     = RGB 255 0 0
-- | #ffff00
yellow::Color
yellow  = RGB 255 255 0
-- | #00ff00
green::Color
green   = RGB 0 255 0
-- | #00ffff
cyan::Color
cyan    = RGB 0 255 255
-- | #0000ff
blue::Color
blue    = RGB 0 0 255
-- | #ff00ff
magenta::Color
magenta = RGB 255 0 255
-- | #ffffff
white::Color
white   = RGB 255 255 255
-- | #7f7f7f
gray::Color
gray    = RGB 127 127 127