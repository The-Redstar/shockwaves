{-|

Module exporting the tools required for custom 'WaveformLUT' implementations.

-}

module Clash.Shockwaves.LUT (
  Waveform,
  WaveformLUT(labelL,styleL,precL,displayL,splitL,structureL),
  WaveformForLUT,

  Translation(..),
  Value,
  WaveStyle(..),
  Prec,
  Structure,
  SubSignal,

) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform