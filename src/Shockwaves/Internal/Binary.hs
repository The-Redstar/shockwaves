{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Shockwaves.Internal.Binary where

import Clash.Prelude
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))
import Shockwaves.Internal.Types


class BitPack a => BinaryPack a where
  binPack :: a -> BinRep

instance BitPack a => BinaryPack a where
  binPack = binPackBV . pack

binPackBV :: KnownNat n => BitVector n -> String
binPackBV (BV @n m i) =
  case natToNum @n @Int of
    0 -> "0"
    _ -> go (natToNum @n @Int) m i []
  where
  go 0 _ _ s = s
  go n m0 v0 s =
    let
      (!v1, !vBit) = quotRem v0 2
      (!m1, !mBit) = quotRem m0 2
      !renderedBit = showBit mBit vBit
    in
      go (n - 1) m1 v1 (renderedBit :       s)

  showBit 0 0 = '0'
  showBit 0 1 = '1'
  showBit _ _ = 'x'