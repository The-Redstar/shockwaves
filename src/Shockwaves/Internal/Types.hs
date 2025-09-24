{-# LANGUAGE NoFieldSelectors #-}


module Shockwaves.Internal.Types where
import Clash.Prelude
import Shockwaves.Style (Color)
import Data.Map as M
import Data.Data (Typeable)

-- some type aliases for clarity
type TypeName = String -- name of a type
type SubSignal = String -- name of a subsignal
type Value = String -- text displayed as the value of a signal
type Prec = Integer -- operator precedence of the value
type Render = Maybe (Value, WaveStyle, Prec)
type BinRep = String -- binary representation of a haskell value (like bitvector, but arbitrarily sized)
type LUTName = TypeName -- reference to a LUT

type TypeMap = Map TypeName WaveformMeta -- map of type metainformation
type LUTMap = Map LUTName LUT -- table of all luts
type LUT = Map BinRep Translation -- single lut



-- Translations
data Translation = Translation (Maybe (Value,WaveStyle,Prec)) [(SubSignal,Translation)] deriving (Show)
data WaveStyle = WSNormal | WSWarn | WSError | WSColor Color deriving (Show)


data NumberFormat = NFDec | NFHex | NFOct | NFBin deriving (Show, Typeable)

newtype Structure = Structure [(SubSignal,Structure)]

data WaveformMeta = Meta
  { trans  :: Translator
  , struct :: Structure
  }

data Translator = Translator Integer TranslatorVariant deriving (Show)

data TranslatorVariant
  = TRef TypeName
  | TSum [(Maybe SubSignal, Translator)]
  | TProduct
    { subs          :: [(SubSignal, Translator)]
    , start        :: Value
    , sep          :: Value
    , stop         :: Value
    , labels       :: [Maybe Value]
    , preci        :: Prec
    , preco        :: Prec
    }
  | TConst Translation
  | TLut LUTName
  | TNumber
    { format :: NumberFormat
    , signed :: Bool
    }
  | TArray
    { sub    :: Translator
    , elems  :: Int
    , start  :: Value
    , sep    :: Value
    , stop   :: Value
    , preci  :: Prec
    , preco  :: Prec}
  | TStyled WaveStyle Translator
  | TMaybe Translator
  deriving (Show)