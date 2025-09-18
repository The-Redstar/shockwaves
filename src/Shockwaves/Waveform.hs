{-# LANGUAGE AllowAmbiguousTypes #-}
module Shockwaves.Waveform where

import Clash.Prelude
import Data.Proxy
import GHC.Generics

newtype Structure = Structure [(SubLabel,Structure)]

type Color = ()

type TypeName = String
type SubLabel = String
type TransLabel = String
type Prec = Integer

data TransStyle = TSNormal | TSWarn | TSError | TSColor Color

data WaveformMeta = Meta
  { trans :: Translator
  , struct :: Structure
  }

data Translation = Translation (Maybe (TransLabel,TransStyle,Prec)) [(SubLabel,Translation)]

class Waveform a where
  translator :: Translator
  -- renderer :: Renderer
  -- splitter :: Splitter
  run  ::       a -> (forall b. Waveform b =>       b -> c) -> [(SubLabel,c)]
  runT :: Proxy a -> (forall b. Waveform b => Proxy b -> c) -> [(SubLabel,c)]
  -- translate :: a -> Translation

  translate :: a -> Translation
  translate x = Translation ren subw
    where
      subw = run x translate
      ren = render x subw

  render :: a -> [(SubLabel,Translation)] -> Maybe (TransLabel,TransStyle,Prec)

  structure :: Structure
  structure = Structure $ runT (Proxy @a) structure'

structure' :: Waveform a => Proxy a -> Structure
structure' (_::Proxy a) = structure @a

data Translator
  = Translator
    { renderer :: Renderer
    , splitter :: Splitter
    , width    :: Integer
    }
  | Ref TypeName


data IntFormat = Dec | Oct | Hex | Bin

data Renderer
  = RLut
  | RProduct
    { start :: TransLabel
    , sep   :: TransLabel
    , stop  :: TransLabel
    , preci :: Prec
    , preco :: Prec
    }
  | RSum
  | RArray
    { start :: TransLabel
    , sep   :: TransLabel
    , stop  :: TransLabel
    , preci :: Prec
    , preco :: Prec
    }
  | RInt
    { signed :: Bool
    , format :: IntFormat
    }

data Splitter
  = SLut
  | SProduct [(Maybe SubLabel, Translator)]
  | SSum [(SubLabel, Translator)]
  | SArray Translator
  | SNoSplit


bitsize :: (BitPack a) => Proxy a -> Integer
bitsize (_ :: Proxy a) = natVal $ Proxy @(BitSize a)

newtype WaveformGeneric a = WaveformGeneric a





newtype WaveformLUT a = WaveformLUT a
instance (WaveformRender a,WaveformSplit a,BitPack a) => Waveform (WaveformLUT a) where
  translator = Translator
    { renderer = rendererR @a
    , splitter = splitterS @a
    , width = bitsize $ Proxy @a
    }

  run (WaveformLUT x) = runS x
  runT _ = runST $ Proxy @a

  translate (WaveformLUT x) = Translation ren subw
    where
      subw = splitS x
      ren = renderR x

  render (WaveformLUT x) _ = renderR x
  
  structure = structureS @a

class WaveformRender a where
  rendererR :: Renderer
  rendererR = RLut

  renderR :: a -> Maybe (TransLabel,TransStyle,Prec)
  default renderR :: (Show a) => a -> Maybe (TransLabel,TransStyle,Prec)
  renderR x = Just (show x,TSNormal,prec @a)
  
  prec :: Prec
  prec = 10


class WaveformSplit a where
  splitterS :: Splitter
  splitterS = SLut

  structureS :: Structure
  structureS = Structure []

  runS  ::       a -> (forall b. Waveform b =>       b -> c) -> [(SubLabel,c)]
  runS _ _ = []
  runST :: Proxy a -> (forall b. Waveform b => Proxy b -> c) -> [(SubLabel,c)]
  runST _ _ = []

  splitS :: a -> [(SubLabel, Translation)]



-- a single constructor yields its rounds

class RunGeneric a where
    runG :: a -> (forall b. Waveform b => b -> c) -> [(SubLabel,c)]
    runGT :: Proxy a -> (forall b. Waveform b => b -> c) -> [(SubLabel, c)]

instance RunGeneric c => RunGeneric (D1 m1 (a :+: bs)) where
    runG M1{unM1=x} = runG x
    runGT _ = [(runGT $ Proxy @c)]

instance RunGeneric c => RunGeneric (D1 m1 (C1 m2 s)) where
    runG M1{unM1=x} = runG x
    runGT _ = runGT $ Proxy @c

instance (RunGeneric a, RunGeneric b) => RunGeneric (a :+: b) where
    runG (L1 x) = runG x
    runG (R1 y) = runG y