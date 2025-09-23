{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Shockwaves.Internal.Binary where

import Clash.Prelude
import Shockwaves.Internal.Types


class BitPack a => BinaryPack a where
  binPack :: a -> BinRep

instance BitPack a => BinaryPack a where
  binPack = undefined -- TODO