{-|

Type definitions for Shockwaves.


-}


{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Clash.Shockwaves.Internal.Types where
import Clash.Prelude hiding (sub)
import Data.Map as M
import Data.Data (Typeable)

import Data.Aeson hiding (Value)
import Data.Colour.SRGB (RGB(..), toSRGB24, Colour)
import Data.Word (Word8)
import Control.DeepSeq (NFData (rnf))
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import Data.Colour.Names (readColourName)
import Data.Maybe (fromJust)

-- some type aliases for clarity
type TypeName = String -- ^ Name of a type.
type SubSignal = String -- ^ Name of a subsignal.
type SignalName = SubSignal -- ^ Name of a signal.
type Value = String -- ^ Text displayed as the value of a signal.
type Prec = Integer -- ^ Operator precedence of the value.
type Render = Maybe (Value, WaveStyle, Prec) -- ^ Rendered value. This can be @Nothing@ is the value does not exists, or a tuple of the text representation, style, and precedence.
type BinRep = String -- ^ Binary representation of a haskell value (like 'BitVector', but arbitrarily sized).
type LUTName = TypeName -- ^ Reference to a LUT.

type SignalMap = Map SignalName TypeName -- ^ Map that links signal names to their types.
type TypeMap = Map TypeName WaveformMeta -- ^ Map that links type names to their information.
type LUTMap = Map LUTName LUT -- ^ Table of LUTs. Usually, the index is a type name, but this is not necessarily the case.
type LUT = Map BinRep Translation -- ^ A lookup table of 'Translation's.

type Color = RGB Word8 -- ^ The color type used in 'WaveStyle'.

-- | Translation of a value.BinRep
-- The translation consists of a 'Render' value (the representation of the value itself)
-- and a list of subsignal translations.
data Translation = Translation (Maybe (Value,WaveStyle,Prec)) [(SubSignal,Translation)] deriving (Show,Generic,ToJSON,NFData)

-- | The style in which a signal should be displayed.
data WaveStyle
  = WSNormal -- ^ The default waveform style.
  | WSWarn -- ^ A warning value.
  | WSError -- ^ An error value. TODO: Errors are propagated by translators.
  | WSColor Color -- ^ A custom color. See "Clash.Shockwaves.Style" for more information.
  deriving (Show, Generic)

instance NFData WaveStyle where
  rnf !_ = ()

-- deriving instance Generic Color -- TODO: move back to custom color class? would be a shame
-- deriving instance NFData Color

-- | Different number formats.
data NumberFormat
  = NFDec -- ^ A decimal value.
  | NFHex -- ^ A hexadecimal value. TODO: Supports partially undefined values.
  | NFOct -- ^ An octal value. TODO: Supports partially undefined values.
  | NFBin -- ^ A binary value. TODO: Supports partially undefined values.
  deriving (Show, Typeable, Generic, NFData)

-- | A structure value that shows what subsignals are present.
newtype Structure = Structure [(SubSignal,Structure)] deriving (Show,Generic,ToJSON)

-- | Type information. This consists of the subsignal hierarchy, and the translator. These values must match.
data WaveformMeta = Meta
  { trans  :: Translator
  , struct :: Structure
  }

-- | A translator. The translator has a width, indicating the number of bits it translates,
-- as well as a 'TranslatorVariant' that determines the translation algorithm.
data Translator = Translator Integer TranslatorVariant deriving (Show)

-- | The translation algorithm used.
data TranslatorVariant
  = TRef TypeName -- ^ Use the translator of a different type. Note that the width value of the 'Translator's should still match.
  | TSum [(Maybe SubSignal, Translator)]
  -- ^ Select one translator to be used based on the first bits of the binary representation. 
  -- If the subsignal name is omitted, no actual subsignal will be shown for this translator.
  | TProduct
    { subs          :: [(SubSignal, Translator)] -- ^ List of fields to translate.
    , start        :: Value -- ^ Text to insert at the start of the value.
    , sep          :: Value -- ^ Text to use to separate values.
    , stop         :: Value -- ^ Text to insert at the end of the value.
    , labels       :: [Maybe Value]
    -- ^ List of labels to insert before each value.
    -- If empty, insert no labels.
    -- Else, the length must match that of @subs@, and provided values are inserted.
    , preci        :: Prec -- ^ Inner precedence: used on subvalues.
    , preco        :: Prec -- ^ Outer precedence: used for the combined value.
    }
  -- ^ Split the binary data into separate fields, translate each of these, and join together the values.
  --
  -- Example:
  -- @
  -- data T = T{a::Bool,b::Bool}
  -- translatorVariantT = TProduct
  --   { subs = [("a",Bool,"b",Bool)],
  --   , start = "T{"
  --   , sep = ","
  --   , stop = "}"
  --   , labels = ["a=","b="]
  --   , preci = 0
  --   , preco = 11
  --   }
  -- @
  | TConst Translation -- ^ A constant translation value. The binary value provided is completely ignored, even if not properly defined.
  | TLut LUTName -- ^ A reference to a lookup table.
  | TNumber
    { format :: NumberFormat -- ^ Format used to display data.
    , signed :: Bool -- ^ Indicates whether the value should be interpreted as signed (two's complement) or not.
    }
  -- ^ A numerical value.
  | TArray
    { sub    :: Translator -- ^ Translator used for all values.
    , len    :: Int -- ^ Length of the array.
    , start  :: Value -- ^ Text inserted at the start of the value.
    , sep    :: Value -- ^ Text to use to separate values.
    , stop   :: Value -- ^ Text to insert at the end of the value.
    , preci  :: Prec -- ^ Inner precedence: used on subvalues.
    , preco  :: Prec -- ^ Outer precedence: used for the combined value.
    }
  -- ^ An array value. This behaves much like 'TProduct', except that no labels are provided, and all fields use the same translator.
  | TStyled WaveStyle Translator -- ^ Apply a style to a translation. Does not change the structure.
  | TMaybe Translator
  -- ^ Translate a value only if the first bit of the binary representation is @1@. If it is @0@, display nothing.
  -- TODO: Verify this is actually useful.
  deriving (Show)



instance IsString WaveStyle where
  fromString s = WSColor . toSRGB24 . fromJust $ (readColourName s :: (Maybe (Colour Double)))


instance ToJSON Translator where
  toJSON (Translator w v) = object ["w" .= w, "v" .= v']
    where v' = case v of
                TRef n -> object ["R" .= n]
                TSum subs -> object ["S" .= toJSON subs]
                TProduct{subs,start,sep,stop,labels,preci,preco} -> object ["P" .= object
                  [ "t" .= toJSON subs
                  , "[" .= start
                  , "," .= sep
                  , "]" .= stop
                  , "n" .= labels
                  , "p" .= preci
                  , "P" .= preco]]
                TConst t -> object ["C" .= toJSON t]
                TLut lut -> object ["L" .= lut]
                TNumber{format,signed} -> object ["N" .= object ["f" .= format, "s" .= signed]]
                TArray{sub,len,start,sep,stop,preci,preco} -> object ["A" .= object 
                  [ "t" .= toJSON sub
                  , "l" .= len
                  , "[" .= start
                  , "," .= sep
                  , "]" .= stop
                  , "p" .= preci
                  , "P" .= preco ]]
                TStyled s t -> object ["X" .= [toJSON s,toJSON t]]
                TMaybe t -> object ["M" .= toJSON t]
instance ToJSON WaveformMeta where
  toJSON Meta {trans,struct} = object ["t" .= trans, "s" .= struct]
instance ToJSON WaveStyle where
  toJSON = \case
    WSNormal  -> "N"
    WSWarn    -> "W"
    WSError   -> "E"
    WSColor (RGB r g b) -> object ["C" .= toJSON [r,g,b,255]]
instance ToJSON NumberFormat where
  toJSON = \case
    NFDec -> "D"
    NFHex -> "H"
    NFOct -> "O"
    NFBin -> "B"

-- instance ToJSON Color where