{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Shockwaves.Waveform where

import Clash.Prelude
import Data.Proxy
import GHC.Generics
import Data.Map hiding (map)
import Data.Char (isAlpha)
import qualified Data.List
import Data.Typeable

newtype Structure = Structure [(SubLabel,Structure)]

type Color = ()

type TypeName = String
type SubLabel = String
type TransLabel = String
type Prec = Integer
type BinRep = String
type LUTLabel = TypeName

type TypeMap = Map TypeName WaveformMeta
type LUTMap = (Map LUTLabel LUT,Map LUTLabel LUT)
type LUT = Map BinRep Translation

data WaveStyle = WSNormal | WSWarn | WSError | WSColor Color

data WaveformMeta = Meta
  { trans :: Translator
  , struct :: Structure
  }

data Translation = Translation (Maybe (TransLabel,WaveStyle,Prec)) [(SubLabel,Translation)]

data Translator
  = Translator
    { renderer :: Renderer
    , splitter :: Splitter
    , width    :: Integer
    }
  | Ref TypeName
  | WithStyle WaveStyle Translator

data IntFormat = Dec | Oct | Hex | Bin

data Renderer
  = RLut LUTLabel
  | RProduct
    { start :: TransLabel
    , sep   :: TransLabel
    , stop  :: TransLabel
    , preci :: Prec
    , preco :: Prec
    , labels:: [SubLabel]
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
  | RConst
    { val :: Maybe (TransLabel,WaveStyle,Prec)
    }

data Splitter
  = SLut LUTLabel
  | SProduct [(SubLabel, Translator)]
  | SSum [(SubLabel, Translator)]
  | SArray Translator
  | SNoSplit


bitsize :: (BitPack a) => Proxy a -> Integer
bitsize (_ :: Proxy a) = natVal $ Proxy @(BitSize a)

typeName :: Typeable a => Proxy a -> TypeName
typeName p = show $ typeRep p --TODO: fix it so it includes the full path

insertIfMissing :: (Ord k) => k -> v -> Map k v -> Map k v
insertIfMissing k v m = if member k m then m else insert k v m

applyStyle :: WaveStyle -> Translation -> Translation
applyStyle s (Translation (Just (l,WSNormal,p)) sb) = Translation (Just (l,s,p)) sb
applyStyle _ t = t

class (BitPack a, Typeable a) => Waveform a where
  meta :: WaveformMeta
  meta = Meta{trans=translator @a, struct = undefined} --TODOstructure @a}

  translator :: Translator
  default translator :: WaveformG (Rep a ()) => Translator
  translator = Translator
    { renderer = rendererG @(Rep a ())
    , splitter = splitterG @(Rep a ()) (styles @a <> Data.List.repeat WSNormal)
    , width = bitsize (Proxy @a)
    }

  structure :: Structure
  default structure :: WaveformG (Rep a ()) => Structure
  structure = Structure $ structureG @(Rep a ())

  addTypes :: TypeMap -> TypeMap
  addTypes tm = addSubTypes @a $ insertIfMissing (typeName (Proxy @a)) (meta @a) tm

  addSubTypes :: TypeMap -> TypeMap
  default addSubTypes :: WaveformG (Rep a ()) => TypeMap -> TypeMap
  addSubTypes = addTypesG @(Rep a ())

  addValue :: a -> LUTMap -> LUTMap
  default addValue :: (Generic a, WaveformG (Rep a ())) => a -> LUTMap -> LUTMap
  addValue x = if hasLUT @a then
                 addValueG @(Rep a ()) (from x)
               else id

  hasLUT :: Bool
  default hasLUT :: WaveformG (Rep a ()) => Bool
  hasLUT = hasLUTG @(Rep a ())

  translate :: a -> Translation
  default translate :: (Generic a, WaveformG (Rep a ())) => a -> Translation
  translate x = if nConstructorsG @(Rep a ()) == 1 then applyStyle (Data.List.head $ styles @a) t else t
    where
      subs = splitG (styles @a) (from x :: Rep a ()) :: [(SubLabel,Translation)]
      ren = render (rendererG @(Rep a ())) subs
      t = Translation ren subs

  styles :: [WaveStyle]
  styles = []


-- fully LUT'd 
class (Typeable a, BitPack a) => WaveformLUT a where
  renderLUT :: a -> Maybe (TransLabel,WaveStyle,Prec)
  default renderLUT :: Show a => a -> Maybe (TransLabel,WaveStyle,Prec)
  renderLUT x = Just (show x, WSNormal, 10) -- is this a sane default prec? hmmm

  structureLUT :: Structure
  default structureLUT :: WaveformG (Rep a ()) => Structure
  structureLUT = Structure $ structureG @(Rep a ())

  splitLUT :: a -> [(SubLabel,Translation)]
  default splitLUT :: WaveformG (Rep a ()) => a -> [(SubLabel,Translation)]
  splitLUT x = splitG (stylesLUT @a) (from x :: Rep a ())

  stylesLUT :: [WaveStyle]
  stylesLUT  = []

instance WaveformLUT a => Waveform a where
  translator = Translator
    { renderer = RLut $ typeName (Proxy @a)
    , splitter = SLut $ typeName (Proxy @a)
    , width = bitsize (Proxy @a)
    }

  addSubTypes = id -- no subtypes

  addValue x (mapR,mapS) = (mapR',mapS')
    where
      Translation r s = translate x
      lbl = typeName (Proxy @a)
      b = bin x
      -- r = renderLUT x
      mapR' = insert lbl (insertIfMissing b r $ findWithDefault Data.Map.empty lbl mapR) mapR 

  hasLUT = True

  translate x = Translation ren subsignals -- TODO handle errors
    where
      subsignals = splitLUT x
      ren = renderLUT x

  styles = stylesLUT @a




-- a single constructor yields its rounds

class WaveformG a where
  translatorG :: [WaveStyle] -> Integer -> Translator
  translatorG = translatorG' @a
  translatorG' :: [WaveStyle] -> Integer -> Translator
  translatorG' s w = Translator
    { renderer = rendererG @a
    , splitter = splitterG @a s
    , width = w
    }
  rendererG :: Renderer
  splitterG :: [WaveStyle] -> Splitter
  splitG :: [WaveStyle] -> a -> [(SubLabel,Translation)]
  structureG :: [(SubLabel,Structure)]
  addTypesG :: TypeMap -> TypeMap
  addValueG :: a -> LUTMap -> LUTMap
  hasLUTG :: Bool

  constructorsG :: [(SubLabel,Translator)]
  constructorsG = undefined

  fieldsG :: [(SubLabel,Translator)]
  fieldsG = undefined

  nConstructorsG :: Integer
  nConstructorsG = undefined


-- void type
instance WaveformG (D1 m1 V1 k) where
  rendererG = RConst{ val = Nothing }
  splitterG _ = SNoSplit
  splitG _ _ = []
  structureG = []
  addTypesG = id
  addValueG _ = id
  hasLUTG = False
  nConstructorsG=0

wrapStyle :: WaveStyle -> Translator -> Translator
wrapStyle WSNormal = id
wrapStyle s = WithStyle s

-- single constructor
instance WaveformG (C1 m2 s k) => WaveformG (D1 m1 (C1 m2 s) k) where
  translatorG [] w = translatorG' @(D1 m1 (C1 m2 s) k) [] w --should never happen
  translatorG (s:ss) w = wrapStyle s $ translatorG' @(D1 m1 (C1 m2 s) k) (s:ss) w
  rendererG = rendererG @(C1 m2 s k)
  splitterG _ = splitterG @(C1 m2 s k) undefined
  splitG _ M1{unM1=c} = splitG @(C1 m2 s k) undefined c
  structureG = s
    where (_,Structure s) = Data.List.head $ structureG @(C1 m2 s k)
  addTypesG = addTypesG @(C1 m2 s k)
  addValueG M1{unM1=c} = addValueG @(C1 m2 s k) c
  hasLUTG = hasLUTG @(C1 m2 s k)
  nConstructorsG=1

-- multiple constructors
instance WaveformG ((a :+: b) k) => WaveformG (D1 m1 (a :+: b) k) where
  rendererG = RSum
  splitterG s = SSum $ wrapStyles $ constructorsG @((a :+: b) k)
    where
      wrapStyles :: [(SubLabel,Translator)] -> [(SubLabel,Translator)]
      wrapStyles = Data.List.zipWith ($) (Data.List.map (\style (sb,trans) -> (sb,wrapStyle style trans)) s)
  splitG ss M1{unM1=x} = Data.List.zipWith (\s (l,t) -> (l,applyStyle s t)) ss $ splitG @((a :+: b) k) undefined x
  structureG = structureG @((a :+: b) k)
  addTypesG = addTypesG @((a :+: b) k)
  addValueG M1{unM1=c} = addValueG @((a :+: b) k) c
  hasLUTG = hasLUTG @((a :+: b) k)
  nConstructorsG = nConstructorsG @((a :+: b) k)

instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :+: b) k) where
  rendererG = undefined
  splitterG = undefined
  splitG _ (L1 x) = splitG undefined x
  splitG _ (R1 x) = splitG undefined x
  structureG = structureG @(a k) <> structureG @(b k)
  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG (L1 x) = addValueG @(a k) x
  addValueG (R1 x) = addValueG @(b k) x
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)
  constructorsG = constructorsG @(a k) <> constructorsG @(b k)
  nConstructorsG = nConstructorsG @(a k) + nConstructorsG @(b k)


-- struct
instance (WaveformG (fields k), KnownSymbol name) => WaveformG (C1 (MetaCons name fix True) fields k) where
  rendererG = RProduct
    { start  = symbolVal (Proxy @name) <> "{"
    , sep    = ", "
    , stop   = "}"
    , preci  = 0
    , preco  = 11
    , labels = Data.List.map fst (fieldsG @(fields k))
    }
  splitterG _ = SProduct (fieldsG @(fields k))
  splitG _ M1{unM1=f} = [(symbolVal (Proxy @name), t)]
    where
      subs = splitG undefined f
      ren = render (rendererG @(C1 (MetaCons name fix True) fields k)) subs
      t = Translation ren subs
  structureG = [(symbolVal (Proxy @name), Structure $ structureG @(fields k))]
  addTypesG = addTypesG @(fields k)
  addValueG M1{unM1=x} = addValueG @(fields k) x
  hasLUTG = hasLUTG @(fields k)
  constructorsG = [
    ( symbolVal (Proxy @name)
    , Translator
      { renderer = rendererG @((C1 (MetaCons name fix True) fields) k)
      , splitter = splitterG @((C1 (MetaCons name fix True) fields) k) undefined
      , width = -1 -- TODO sum $ (...field widths...)
      }
    )]
  nConstructorsG = 1


-- add parentheses around operators when not used as a binary operator
safeName :: String -> String
safeName n = if isAlpha $ Data.List.head n then n else "("<>n<>")"

-- applicative product
instance (WaveformG (fields k), KnownSymbol name) => WaveformG (C1 (MetaCons name fix False) fields k) where
  rendererG = RProduct
    { start  = safeName (symbolVal (Proxy @name)) <> " "
    , sep    = " "
    , stop   = ""
    , preci  = 10
    , preco  = 10
    , labels = []
    }
  splitterG _ = SProduct (fieldsG @(fields k))
  splitG _ M1{unM1=f} = [(symbolVal (Proxy @name), t)]
    where
      subs = splitG undefined f
      ren = render (rendererG @(C1 (MetaCons name fix True) fields k)) subs
      t = Translation ren subs
  structureG = [(symbolVal (Proxy @name), Structure $ structureG @(fields k))]
  addTypesG = addTypesG @(fields k)
  addValueG M1{unM1=x} = addValueG @(fields k) x
  hasLUTG = hasLUTG @(fields k)
  constructorsG = [
    ( symbolVal (Proxy @name)
    , Translator
      { renderer = rendererG @((C1 (MetaCons name fix True) fields) k)
      , splitter = splitterG @((C1 (MetaCons name fix True) fields) k) undefined
      , width = -1 -- TODO sum $ (...field widths...)
      }
    )]
  nConstructorsG = 1

-- multiple fields
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :*: b) k) where
  rendererG = undefined
  splitterG = undefined
  splitG _ (x :*: y) = splitG undefined x <> splitG undefined y
  structureG = structureG @(a k) <> structureG @(b k)
  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG (x :*: y) = addValueG x . addValueG y
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)
  fieldsG = fieldsG @(a k) <> fieldsG @(b k)

-- struct field
instance (Waveform t, KnownSymbol fname) => WaveformG (S1 (MetaSel (Just fname) p q r) (Rec0 t) k) where
  rendererG = undefined
  splitterG = undefined
  splitG _ M1{unM1=K1{unK1=x}} = [(symbolVal (Proxy @fname),translate x)]
  structureG = [(symbolVal (Proxy @fname), structure @t)]
  addTypesG = addTypes @t
  addValueG M1{unM1=K1{unK1=x}} = addValue @t x
  hasLUTG = hasLUT @t
  fieldsG = [(symbolVal (Proxy @fname), Ref $ typeName (Proxy @t))]

-- unnamed field
instance (Waveform t) => WaveformG (S1 (MetaSel Nothing p q r) (Rec0 t) k) where
  rendererG = undefined
  splitterG = undefined
  splitG _ M1{unM1=K1{unK1=x}} = [("",translate x)]
  structureG = [("", structure @t)]
  addTypesG = addTypes @t
  addValueG M1{unM1=K1{unK1=x}} = addValue @t x
  hasLUTG = hasLUT @t
  fieldsG = [("", Ref $ typeName (Proxy @t))]







-- renderers

render :: Renderer -> [(SubLabel,Translation)] -> Maybe (TransLabel,WaveStyle,Prec)
render = undefined -- TODO















-- implementations

instance (KnownNat n) => Waveform (Unsigned n) where
  translator = Translator
    { renderer = RInt{signed=False,format=Dec}
    , splitter = SNoSplit
    , width = natVal (Proxy @n)
    }
  translate = undefined -- TODO
  hasLUT = False
  addValue _ = id
  addSubTypes = id