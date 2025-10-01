
module Shockwaves.Style (
  module Colors,
  WaveStyle(..),
  Color,
  RGB(..),
  Word8,
) where
import Shockwaves.Internal.Types (WaveStyle(..),Color)
import Data.Colour.SRGB (RGB(..))
import Data.Word (Word8)

import Data.Colour.Names as Colors