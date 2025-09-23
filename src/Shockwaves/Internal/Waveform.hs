{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Shockwaves.Internal.Waveform where
import Clash.Prelude
import GHC.Generics
import Data.Proxy
import Data.Typeable
import Shockwaves.Internal.Types
import Shockwaves.Internal.Binary
import Shockwaves.Internal.Render
import Data.Map as M
import qualified Data.List as L
import Data.Char (isAlpha)



bitsize :: (BitPack a) => Proxy a -> Integer
bitsize (_ :: Proxy a) = natVal $ Proxy @(BitSize a)

typeName :: Typeable a => Proxy a -> TypeName
typeName p = show $ typeRep p --TODO: fix it so it includes the full path

insertIfMissing :: (Ord k) => k -> v -> Map k v -> Map k v
insertIfMissing k v m = if member k m then m else M.insert k v m



wrapStyle :: WaveStyle -> Translator -> Translator
wrapStyle WSNormal t = t
wrapStyle s (Translator w v) = Translator w $ TStyled s (Translator w v)

filterSignals :: [(SubSignal,Translation)] -> [(SubSignal,Translation)]
filterSignals = L.filter ((/="") . fst)

-- add parentheses around operators when not used as a binary operator
safeName :: String -> String
safeName n = if isAlpha $ L.head n then n else parenthesize n

class (KnownSymbol s) => QuickSymbol s where
  sym :: String
  sym = symbolVal (Proxy @s)
instance (KnownSymbol s) => QuickSymbol s




class (BitPack a, Typeable a) => Waveform a where
  meta :: WaveformMeta
  meta = Meta
    { trans = translator @a
    , struct = structure @a
    }

  structure :: Structure
  default structure :: (WaveformG (Rep a ())) => Structure
  structure = structureG @(Rep a ())
  
  translator :: Translator
  default translator :: (WaveformG (Rep a ())) => Translator
  translator = translatorG @(Rep a ()) (width @a) (styles' @a) 
  
  translate :: a -> Translation
  default translate :: (Generic a, WaveformG (Rep a ())) => a -> Translation
  translate x = Translation ren $ filterSignals subs
    where
      subs = splitG (from x :: Rep a ())
      ren = render (translator @a) subs

  addTypes :: TypeMap -> TypeMap
  addTypes tm = if M.member self tm then
                  tm
                else
                  addSubtypes @a $ M.insert self (meta @a) tm
    where self = typeName (Proxy @a)
    --addSubtypes @a $ insertIfMissing (typeName (Proxy @a)) (meta @a) tm

  addSubtypes :: TypeMap -> TypeMap
  default addSubtypes :: (WaveformG (Rep a ())) => TypeMap -> TypeMap
  addSubtypes = addTypesG @(Rep a ())

  addValue :: a -> LUTMap -> LUTMap
  default addValue :: (Generic a, WaveformG (Rep a ())) => a -> LUTMap -> LUTMap
  addValue x = if hasLUT @a then
                 addValueG (from x :: Rep a ())
               else
                 id

  hasLUT :: Bool
  default hasLUT :: (WaveformG (Rep a ())) => Bool
  hasLUT = hasLUTG @(Rep a())

  width :: Integer
  width = bitsize (Proxy @a)

  styles :: [WaveStyle]
  styles = []
  
  styles' :: [WaveStyle]
  styles' = styles @a <> L.repeat WSNormal


class WaveformG a where
  structureG :: Structure
  structureG = Structure $ structureG' @a
  structureG' :: [(SubSignal,Structure)]
  translatorG :: Integer -> [WaveStyle] -> Translator
  splitG :: a -> [(SubSignal,Translation)]
  addTypesG :: TypeMap -> TypeMap
  addValueG :: a -> LUTMap -> LUTMap
  hasLUTG :: Bool

  widthG :: Integer
  widthG = undefined

  constructorsG :: [(SubSignal,Translator,Structure)]
  constructorsG = undefined
  fieldsG :: [(SubSignal,Translator)]
  fieldsG = undefined



-- void type
instance WaveformG (D1 m1 V1 k) where
  structureG' = []
  translatorG _ _ = Translator 0 $ TConst $ Translation Nothing []
  splitG _ = []
  addTypesG = id
  addValueG _ = id
  hasLUTG = False
  -- widthG = 0

-- single constructor
instance WaveformG (C1 m2 s k) => WaveformG (D1 m1 (C1 m2 s) k) where
  structureG' = L.map (\(n,_t,s) -> (n,s)) $ constructorsG @(C1 m2 s k)
  translatorG _ (sty:_) = wrapStyle sty $ translatorG @(C1 m2 s k) undefined undefined
  translatorG _ [] = undefined
  splitG M1{unM1=x} = splitG x
  addTypesG = addTypesG @(C1 m2 s k)
  addValueG M1{unM1=x} = addValueG @(C1 m2 s k) x
  hasLUTG = hasLUTG @(C1 m2 s k)
  -- widthG = widthG @(C1 m2 s k)


-- multiple constructors
instance WaveformG ((a :+: b) k) => WaveformG (D1 m1 (a :+: b) k) where
  structureG' = structureG' @((a :+: b) k)
  translatorG w stys = Translator w $ TSum $ L.zipWith (\sty (n,t,_s) ->(Just n,wrapStyle sty t)) stys $ constructorsG @((a :+: b) k)
  splitG M1{unM1=x} = splitG x
  addTypesG = addTypesG @((a :+: b) k)
  addValueG M1{unM1=x} = addValueG x
  hasLUTG = hasLUTG @((a :+: b) k)
  -- widthG = (2log length constructorsG @((a :+: b) k)) + widthG @((a :+: b) k)

instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :+: b) k) where
  structureG' = structureG' @(a k) <> structureG' @(b k)
  translatorG = undefined
  splitG (L1 x) = splitG x
  splitG (R1 y) = splitG y
  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG (L1 x) = addValueG x
  addValueG (R1 y) = addValueG y
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)
  -- widthG = max (widthG @(a k)) (widthG @(b k))

  constructorsG = constructorsG @(a k) <> constructorsG @(b k)

enumLabel :: [(SubSignal,a)] -> [(SubSignal,a)]
enumLabel = L.zipWith enumLabel' [(0::Integer)..]
  where enumLabel' i t = case t of
                            ("",x) -> (show i,x)
                            x      -> x

-- struct
instance (WaveformG (fields k), KnownSymbol name) => WaveformG (C1 (MetaCons name fix True) fields k) where
  structureG' = [(symbolVal (Proxy @name), Structure $ enumLabel $ structureG' @(fields k))]
  translatorG _ _ = Translator (widthG @(fields k)) $ TProduct
    { start  = sym @name <> "{"
    , sep    = ", "
    , stop   = "}"
    , preci  = 0
    , preco  = 11
    , labels = L.map (Just . (<> " = ") . fst) (fieldsG @(fields k))
    , subs   = enumLabel $ fieldsG @(fields k)
    }
  splitG M1{unM1=x} =  [(sym @name, translation)]
    where
      subs = enumLabel $ splitG x
      ren = render (translatorG @(C1 (MetaCons name fix False) fields k) undefined undefined) subs
      translation = Translation ren $ filterSignals subs
  addTypesG = addTypesG @(fields k)
  addValueG M1{unM1=x} = addValueG x
  hasLUTG = hasLUTG @(fields k)

  constructorsG = [
    ( sym @name
    , translatorG @(C1 (MetaCons name fix True) fields k) undefined undefined
    , structureG @(C1 (MetaCons name fix True) fields k)
    )]




-- applicative product
instance (WaveformG (fields k), KnownSymbol name) => WaveformG (C1 (MetaCons name fix False) fields k) where
  structureG' = [(sym @name, Structure $ enumLabel $ structureG' @(fields k))]
  translatorG _ _ = Translator (widthG @(fields k)) $ TProduct
    { start  = case fieldsG @(fields k) of
                 [] -> sname
                 _  -> sname <> " " --TODO: only add space if no parameters
    , sep    = " "
    , stop   = ""
    , preci  = 10
    , preco  = case fieldsG @(fields k) of
                 [] -> 11
                 _  -> 10
    , labels = []
    , subs = enumLabel $ fieldsG @(fields k)
    }
    where sname = safeName (sym @name)
  splitG M1{unM1=x} = [(sym @name, translation)]
    where
      subs = enumLabel $ splitG x
      ren = render (translatorG @(C1 (MetaCons name fix False) fields k) undefined undefined) subs
      translation = Translation ren $ filterSignals subs
  addTypesG = addTypesG @(fields k)
  addValueG M1{unM1=x} = addValueG @(fields k) x
  hasLUTG = hasLUTG @(fields k)

  constructorsG = [
    ( sym @name
    , translatorG @(C1 (MetaCons name fix False) fields k) undefined undefined
    , structureG @(C1 (MetaCons name fix False) fields k)
    )]


-- no fields
instance WaveformG (U1 k) where
  structureG' = []
  translatorG = undefined
  splitG _ = []
  addTypesG = id
  addValueG _ = id
  hasLUTG = False

  fieldsG = []
  widthG = 0

-- multiple fields
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :*: b) k) where
  structureG' = structureG' @(a k) <> structureG' @(b k)
  translatorG = undefined
  splitG (x :*: y) = splitG x <> splitG y 
  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG (x :*: y) = addValueG x . addValueG y
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)

  fieldsG = fieldsG @(a k) <> fieldsG @(b k)
  widthG = widthG @(a k) + widthG @(b k)

-- struct field
instance (Waveform t, KnownSymbol name) => WaveformG (S1 (MetaSel (Just name) p q r) (Rec0 t) k) where
  structureG' = [(sym @name, structure @t)]
  translatorG _ = undefined
  splitG M1{unM1=K1{unK1=x}} = [(sym @name, translate x)]
  addTypesG = addTypes @t
  addValueG M1{unM1=K1{unK1=x}} = addValue @t x
  hasLUTG = hasLUT @t

  fieldsG = [(sym @name, Translator (width @t) $ TRef $ typeName (Proxy @t))]
  widthG = width @t

-- unnamed field
instance (Waveform t) => WaveformG (S1 (MetaSel Nothing p q r) (Rec0 t) k) where
  structureG' = [("", structure @t)]
  translatorG _ = undefined
  splitG M1{unM1=K1{unK1=x}} = [("", translate x)]
  addTypesG = addTypes @t
  addValueG M1{unM1=K1{unK1=x}} = addValue @t x
  hasLUTG = hasLUT @t

  fieldsG = [("", Translator (width @t) $ TRef $ typeName (Proxy @t))]
  widthG = width @t





class (Typeable a, BitPack a) => WaveformLUT a where
  structureL :: Structure
  
  translateL :: a -> Translation
  
  splitL :: a -> [(SubSignal,Translation)]
  default splitL :: (Generic a, WaveformG (Rep a ())) => a -> [(SubSignal,Translation)]
  splitL x = splitG (from x :: Rep a ())
  
  displayL :: a -> Maybe (Value,WaveStyle,Prec)
  displayL x = Just (labelL x, styleL x, precL x)
  
  labelL :: a -> Value
  default labelL :: Show a => a -> Value
  labelL = show

  precL :: a -> Prec
  precL _ = undefined -- TODO
  
  styleL :: a -> WaveStyle
  styleL _ = WSNormal


instance (WaveformLUT a, BitPack a, Typeable a) => Waveform a where
  structure = structureL @a
  translator = Translator (width @a) $ TLut $ typeName (Proxy @a)
  translate = translateL @a

  addSubtypes = id

  addValue x m = M.insert ty (insertIfMissing bin trans $ M.findWithDefault M.empty ty m) m
    where
      ty = typeName (Proxy @a)
      bin = binPack x
      trans = translate x

  hasLUT = True

  width :: Integer
  width = bitsize (Proxy @a)

  styles :: [WaveStyle]
  styles = []
  
  styles' :: [WaveStyle]
  styles' = styles @a <> L.repeat WSNormal


class (Generic a) => PrecG a where
  precG :: a -> Prec
  nFields :: Integer
  nFields = undefined

-- get constructor(s)
instance PrecG (c k) => PrecG (D1 m1 c k) where
  precG M1{unM1=x} = precG x

-- no constructors (void tpye)
instance PrecG (V1 k) where
  precG _ = 11

-- multiple constructors
instance (PrecG (a k), PrecG (b k)) => PrecG ((a :+: b) k) where
  precG (L1 x) = precG x
  precG (R1 y) = precG y

instance (PrecG (fields k), PrecF fix) => PrecG (C1 (MetaCons name fix True) fields k) where
  precG _ = if nFields @(fields k) == 0 then 11 else precF @fix--prec $ fromSing $ sing @fix


-- count fields
instance PrecG (U1 k) where
  precG = undefined
  nFields = 0

instance (PrecG (a k), PrecG (b k)) => PrecG ((a :*: b) k) where
  precG = undefined
  nFields = nFields @(a k) + nFields @(b k)

instance PrecG (S1 (MetaSel n p q r) t k) where
  precG = undefined
  nFields = 1


class PrecF (f::FixityI) where
  precF :: Integer
instance PrecF PrefixI where
  precF = 10
instance (KnownNat p) => PrecF (InfixI a p) where
  precF = natVal (Proxy @p)