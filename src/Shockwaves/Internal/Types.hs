{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Shockwaves.Internal.Types where
import Clash.Prelude hiding (sub)
import Shockwaves.Style (Color)
import Data.Map as M
import Data.Data (Typeable)

import Data.Aeson hiding (Value)
import Data.Colour.SRGB (RGB(..))
import Control.DeepSeq (NFData)

-- some type aliases for clarity
type TypeName = String -- name of a type
type SubSignal = String -- name of a subsignal
type SignalName = SubSignal -- name of a signal
type Value = String -- text displayed as the value of a signal
type Prec = Integer -- operator precedence of the value
type Render = Maybe (Value, WaveStyle, Prec)
type BinRep = String -- binary representation of a haskell value (like bitvector, but arbitrarily sized)
type LUTName = TypeName -- reference to a LUT

type SignalMap = Map SignalName TypeName
type TypeMap = Map TypeName WaveformMeta -- map of type metainformation
type LUTMap = Map LUTName LUT -- table of all luts
type LUT = Map BinRep Translation -- single lut



-- Translations
data Translation = Translation (Maybe (Value,WaveStyle,Prec)) [(SubSignal,Translation)] deriving (Show,Generic,ToJSON,NFData)
data WaveStyle = WSNormal | WSWarn | WSError | WSColor Color deriving (Show, Generic, NFData)

deriving instance Generic Color -- TODO: move back to custom color class? would be a shame
deriving instance NFData Color


data NumberFormat = NFDec | NFHex | NFOct | NFBin deriving (Show, Typeable, Generic, NFData)

newtype Structure = Structure [(SubSignal,Structure)] deriving (Show,Generic,ToJSON)

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
    , len    :: Int
    , start  :: Value
    , sep    :: Value
    , stop   :: Value
    , preci  :: Prec
    , preco  :: Prec}
  | TStyled WaveStyle Translator
  | TMaybe Translator
  deriving (Show)




-- [SigMap,TypeMap,] TypeMeta, Structure, Translator, [LUTMap, LUT], Translation, Render
-- instance ToJSON Structure where
--   toJSON = undefined
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