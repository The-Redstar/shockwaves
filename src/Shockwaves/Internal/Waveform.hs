{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

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

-- for standard type instances
import Data.Int             (Int8,Int16,Int32,Int64)
import Data.Word            (Word8,Word16,Word32,Word64)

-- import Clash.Num.Zeroing    (Zeroing, fromZeroing)
-- import Clash.Num.Wrapping   (Wrapping, fromWrapping)
-- import Clash.Num.Saturating (Saturating, fromSaturating)
-- import Clash.Num.Overflowing(Overflowing, fromOverflowing)
-- import Clash.Num.Erroring   (Erroring, fromErroring)
-- import Data.Functor.Product (Product)
-- import Data.Functor.Sum (Sum)
-- import Data.Functor.Compose (Compose)
import Data.Complex         (Complex)
import Data.Ord             (Down)
import Data.Functor.Identity(Identity)



bitsize :: (BitPack a) => Proxy a -> Integer
bitsize (_ :: Proxy a) = natVal $ Proxy @(BitSize a)

typeName :: Typeable a => Proxy a -> TypeName
typeName p = show (typeRepFingerprint r) <> ":" <> (show r) --TODO: fix it so it includes the full path
  where r = typeRep p

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





----------------------------------------------- WAVEFORM --------------------------------------


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









------------------------------------------- GENERIC -------------------------------------

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
instance (WaveformG (C1 m2 s k), WaveformG (s k)) => WaveformG (D1 m1 (C1 m2 s) k) where
  structureG' = L.map (\(n,_t,s) -> (n,s)) $ constructorsG @(C1 m2 s k)
  translatorG _ (sty:_) = wrapStyle sty $ translatorG @(C1 m2 s k) undefined undefined
  translatorG _ [] = undefined
  splitG M1{unM1=M1{unM1=x}} = enumLabel $ splitG x
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
      ren = render (translatorG @(C1 (MetaCons name fix True) fields k) undefined undefined) subs
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
instance (WaveformG (fields k), KnownSymbol name, PrecF fix) => WaveformG (C1 (MetaCons name fix False) fields k) where
  structureG' = [(sym @name, Structure $ enumLabel $ structureG' @(fields k))]
  translatorG _ _ = if isOperator then
      Translator (widthG @(fields k)) $ TProduct
      { start  = ""
      , sep    = " " <> sym @name <> " "
      , stop   = ""
      , preci  = precF @fix
      , preco  = precF @fix
      , labels = []
      , subs = enumLabel $ fieldsG @(fields k)
      }
    else
      Translator (widthG @(fields k)) $ TProduct
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
    where
      sname = safeName (sym @name)
      isOperator = not (isAlpha . L.head $ sym @name) && (L.length (fieldsG @(fields k)) == 2)

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











------------------------------------------------ LUTS --------------------------------------------------------

class (Typeable a, BitPack a) => WaveformLUT a where
  structureL :: Structure
  default structureL :: (WaveformG (Rep a ())) => Structure
  structureL = structureG @(Rep a ())

  translateL :: a -> Translation
  translateL x = Translation (displayL x) (splitL x)

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

newtype WaveformForLUT a = WfLUT a deriving (Generic,BitPack,Typeable)

instance (WaveformLUT a, BitPack a, Typeable a) => Waveform (WaveformForLUT a) where
  structure = structureL @a
  translator = Translator (width @(WaveformForLUT a)) $ TLut $ typeName (Proxy @a)
  translate (WfLUT x) = translateL x

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
  styles' = styles @(WaveformForLUT a) <> L.repeat WSNormal





----------------------------------------------- PREC ----------------------------------

-- class (Generic a) => PrecG a where
--   precG :: a -> Prec
--   nFields :: Integer
--   nFields = undefined

-- -- get constructor(s)
-- instance PrecG (c k) => PrecG (D1 m1 c k) where
--   precG M1{unM1=x} = precG x

-- -- no constructors (void tpye)
-- instance PrecG (V1 k) where
--   precG _ = 11

-- -- multiple constructors
-- instance (PrecG (a k), PrecG (b k)) => PrecG ((a :+: b) k) where
--   precG (L1 x) = precG x
--   precG (R1 y) = precG y

-- instance (PrecG (fields k), PrecF fix) => PrecG (C1 (MetaCons name fix True) fields k) where
--   precG _ = if nFields @(fields k) == 0 then 11 else precF @fix--prec $ fromSing $ sing @fix


-- -- count fields
-- instance PrecG (U1 k) where
--   precG = undefined
--   nFields = 0

-- instance (PrecG (a k), PrecG (b k)) => PrecG ((a :*: b) k) where
--   precG = undefined
--   nFields = nFields @(a k) + nFields @(b k)

-- instance PrecG (S1 (MetaSel n p q r) t k) where
--   precG = undefined
--   nFields = 1


class PrecF (f::FixityI) where
  precF :: Integer
instance PrecF PrefixI where
  precF = 10
instance (KnownNat p) => PrecF (InfixI a p) where
  precF = natVal (Proxy @p)



---------------------------------------- OTHER VARIANTS -----------------------------------

-- CONST

class (BitPack a, Typeable a) => WaveformConst a where
  constTrans :: Render

newtype WaveformForConst a = WfConst a deriving (Generic,BitPack,Typeable)

instance (WaveformConst a, BitPack a, Typeable a) => Waveform (WaveformForConst a) where
  structure = Structure []
  translator = Translator 0 $ TConst $ Translation (constTrans @a) []
  translate _ = Translation (constTrans @a) []
  addSubtypes = id
  addValue _ = id
  hasLUT = False


-- NUMBERS


newtype WaveformForNumber (b::Bool) (f::NumberFormat) a = WfNum a deriving (Generic,BitPack,Typeable)

instance (BitPack a, Typeable a, Typeable s, Typeable f, KnownBool s, KnownNFormat f, Integral a) => Waveform (WaveformForNumber (s::Bool) (f::NumberFormat) a) where
  structure = Structure []
  translator = Translator 0 $ TNumber{signed = boolVal (Proxy @s), format = formatVal (Proxy @f)}
  translate (WfNum x) = case formatVal $ Proxy @f of
    NFDec -> Translation (Just (show $ toInteger x,WSNormal,11)) []
    _ -> undefined -- TODO; other formats are per-bit
  addSubtypes = id
  addValue _ = id
  hasLUT = False

class KnownBool (b::Bool) where
  boolVal :: forall proxy. proxy b -> Bool
instance KnownBool True where
  boolVal _ = True
instance KnownBool False where
  boolVal _ = False

class KnownNFormat (f::NumberFormat) where
  formatVal :: forall proxy. proxy f -> NumberFormat
instance KnownNFormat NFDec where
  formatVal _ = NFDec
instance KnownNFormat NFHex where
  formatVal _ = NFHex
instance KnownNFormat NFOct where
  formatVal _ = NFOct
instance KnownNFormat NFBin where
  formatVal _ = NFBin

---------------------------------------- IMPLEMENTATIONS ----------------------------------

-- TUPLES

-- for i in range(2,12):
-- 	v = [f"a{j}" for j in range(i)]
-- 	c = ",".join("Waveform "+k for k in v)
-- 	vs = ",".join(v)
-- 	st = ",".join(f"(\"{i}\",structure @{k})" for i,k in enumerate(v))
-- 	sb = ",".join(f"(\"{i}\",translator @{k})" for i,k in enumerate(v))
-- 	print(f"""instance ({c}) => Waveform ({vs}) where
--   structure = Structure [{st}]
--   translator = Translator (width @({vs})) $ TProduct{{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[{sb}]}}
-- """)


instance (Waveform a0,Waveform a1) => Waveform (a0,a1) where
  structure = Structure [("0",structure @a0),("1",structure @a1)]
  translator = Translator (width @(a0,a1)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1)]}

instance (Waveform a0,Waveform a1,Waveform a2) => Waveform (a0,a1,a2) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2)]
  translator = Translator (width @(a0,a1,a2)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3) => Waveform (a0,a1,a2,a3) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3)]
  translator = Translator (width @(a0,a1,a2,a3)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4) => Waveform (a0,a1,a2,a3,a4) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4)]
  translator = Translator (width @(a0,a1,a2,a3,a4)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5) => Waveform (a0,a1,a2,a3,a4,a5) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6) => Waveform (a0,a1,a2,a3,a4,a5,a6) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5),("6",structure @a6)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5),("6",translator @a6)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5),("6",structure @a6),("7",structure @a7)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5),("6",translator @a6),("7",translator @a7)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5),("6",structure @a6),("7",structure @a7),("8",structure @a8)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5),("6",translator @a6),("7",translator @a7),("8",translator @a8)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5),("6",structure @a6),("7",structure @a7),("8",structure @a8),("9",structure @a9)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5),("6",translator @a6),("7",translator @a7),("8",translator @a8),("9",translator @a9)]}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9,Waveform a10) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  structure = Structure [("0",structure @a0),("1",structure @a1),("2",structure @a2),("3",structure @a3),("4",structure @a4),("5",structure @a5),("6",structure @a6),("7",structure @a7),("8",structure @a8),("9",structure @a9),("10",structure @a10)]
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,subs=[("0",translator @a0),("1",translator @a1),("2",translator @a2),("3",translator @a3),("4",translator @a4),("5",translator @a5),("6",translator @a6),("7",translator @a7),("8",translator @a8),("9",translator @a9),("10",translator @a10)]}



-- INSTANCES FOR OTHER STANDARD HASKELL TYPES

instance WaveformConst () where
  constTrans = Just ("()",WSNormal,11)

instance Waveform Bool where
  structure = Structure []
  translator = Translator 1 $ TSum
    [ (Nothing,Translator 0 $ TConst $ Translation (Just ("False",WSNormal,11)) [])
    , (Nothing,Translator 0 $ TConst $ Translation (Just ("True",WSNormal,11)) [])]
  addSubtypes = id
  addValue _ = id

instance (Waveform a) => Waveform (Maybe a) where
  -- structure = Structure [("Just.0",structure @a)]
  -- translator = Translator (width @a +1) $ TSum [(Nothing,Translator 0 $ TConst $ Translation (Just ("Nothing",WSNormal,11)) []),(Just "Just.0",translator @a)]
  -- translate x = Translation ren $ filterSignals subs
  --   where
  --     subs = case x of
  --       Just y -> [("Just.0", ???)]
  --       Nothing -> [("Nothing",Translation (Just ("Nothing",WSNormal,11)) [])]
  --     ren = render (translator @a) subs

instance (Waveform a, Waveform b) => Waveform (Either a b)

instance (BitPack Char) => WaveformLUT Char where
  structureL = Structure []
  splitL _ = []
deriving via WaveformForLUT Char instance (BitPack Char) => Waveform Char

instance WaveformLUT Bit
deriving via WaveformForLUT Bit instance Waveform Bit

instance WaveformLUT Double where
  structureL = Structure []
  splitL _ = []
deriving via WaveformForLUT Double instance Waveform Double

instance WaveformLUT Float where
  structureL = Structure []
  splitL _ = []
deriving via WaveformForLUT Float instance Waveform Float

deriving via WaveformForNumber True NFDec Int instance Waveform Int
deriving via WaveformForNumber True NFDec Int8 instance Waveform Int8
deriving via WaveformForNumber True NFDec Int16 instance Waveform Int16
deriving via WaveformForNumber True NFDec Int32 instance Waveform Int32
deriving via WaveformForNumber True NFDec Int64 instance Waveform Int64

instance Waveform Ordering

deriving via WaveformForNumber False NFDec Word instance Waveform Word
deriving via WaveformForNumber False NFDec Word8 instance Waveform Word8
deriving via WaveformForNumber False NFDec Word16 instance Waveform Word16
deriving via WaveformForNumber False NFDec Word32 instance Waveform Word32
deriving via WaveformForNumber False NFDec Word64 instance Waveform Word64

-- instance Display CUShort where
-- deriving via NoSplit CUShort instance Split CUShort

-- instance Display Half where
-- deriving via NoSplit Half instance Split Half

deriving via WaveformForNumber True NFDec (Signed n) instance (KnownNat n) => Waveform (Signed n)
deriving via WaveformForNumber False NFDec (Unsigned n) instance (KnownNat n) =>  Waveform (Unsigned n)
deriving via WaveformForNumber False NFDec (Index n) instance (1 <= n, KnownNat n) => Waveform (Index n)

instance Waveform a => Waveform (Complex a)

instance Waveform a => Waveform (Down a)

instance Waveform a => Waveform (Identity a)

-- TODO
-- instance (Display a, Split a) => Split (Zeroing a) where
--     structure = VICompound [("0",structure @a)]
--     split z _ = [str "0" $ translate $ fromZeroing z]

-- instance (Display a, Split a) => Split (Wrapping a) where
--     structure = VICompound [("0",structure @a)]
--     split z _ = [str "0" $ translate $ fromWrapping z]

-- instance (Display a, Split a) => Split (Saturating a) where
--     structure = VICompound [("0",structure @a)]
--     split z _ = [str "0" $ translate $ fromSaturating z]

-- instance (Display a, Split a) => Split (Overflowing a) where
--     structure = VICompound [("0",structure @a)]
--     split z _ = [str "0" $ translate $ fromOverflowing z]

-- instance (Display a, Split a) => Split (Erroring a) where
--     structure = VICompound [("0",structure @a)]
--     split z _ = [str "0" $ translate $ fromErroring z]

instance (KnownNat n, Waveform a) => Waveform (Vec n a) where
  structure = Structure $ L.map (\i -> (show i, structure @a)) [0 .. natVal (Proxy @n) - 1]
  translator = Translator (width @(Vec n a)) $ if natVal (Proxy @n) /= 0 then
    TArray
      { start = ""
      , sep = " :> "
      , stop = " :> Nil"
      , preci = 5
      , preco = 5
      , len = fromIntegral $ natVal (Proxy @n)
      , sub = Translator (width @a) $ TRef $ typeName (Proxy @a)
      }
    else
      TConst $ Translation (Just ("Nil",WSNormal,11)) []
  translate v = Translation ren subs
    where
      subs = L.zipWith (\i x -> (show i,translate x)) [(0::Int)..] $ Clash.Prelude.toList v
      ren = render (translator @(Vec n a)) subs
  addSubtypes = addTypes @a
  addValue v = L.foldl (.) id $ L.map addValue $ Clash.Prelude.toList v
  hasLUT = hasLUT @a



deriving via WaveformForNumber False NFBin (BitVector n) instance (KnownNat n) => Waveform (BitVector n)

-- instance (Waveform a) => Waveform (Const a b)

instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r) => WaveformLUT (Fixed r i f) where
  structureL = Structure []
  splitL _ = []
deriving via WaveformForLUT (Fixed r i f)
  instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r) => Waveform (Fixed r i f)

-- instance (Waveform (f a), Waveform (g a)) => Waveform (Product f g a)
-- instance (Waveform (f a), Waveform (g a)) => Waveform (Sum f g a)
-- instance (Waveform (f (g a)), Waveform (g a)) => Waveform (Compose f g a)

instance (KnownNat n, BitPack (SNat n)) => WaveformConst (SNat n) where
  constTrans = Just (show $ natVal $ Proxy @n, WSNormal, 11)
deriving via WaveformForConst (SNat n) instance (KnownNat n, BitPack (SNat n)) => Waveform (SNat n)

-- instance (BitPack (Proxy a), Typeable a) => Waveform (Proxy a)



-- the monster that is RTree :/
type family RTreeIsLeaf d where
  RTreeIsLeaf 0 = True
  RTreeIsLeaf d = False

instance (Waveform a, KnownNat d, WaveformRTree (RTreeIsLeaf d) d a) => Waveform (RTree d a) where
  structure = structureRTree (natVal $ Proxy @d) (structure @a)
    where
      structureRTree :: Integer -> Structure -> Structure
      structureRTree d' sa = Structure $ if d'==0 then [("0",sa)] else [("left",tree'),("right",tree')]
          where tree' = structureRTree (d'-1) sa
  translator = Translator (width @(RTree d a)) $ if natVal (Proxy @d) == 0 then
      TProduct
        { start = "BR "
        , sep = ""
        , stop = ""
        , labels = []
        , preci = 10
        , preco = 10
        , subs = [("0",Translator (width @a) $ TRef $ typeName (Proxy @a))]
        }
    else
      TProduct
        { start = "<"
        , sep = ","
        , stop = ">"
        , labels = []
        , preci = 0
        , preco = 11
        , subs = [("left",tsub),("right",tsub)]
        }
    where tsub = Translator (width @(RTree d a)) $ TRef $ typeName (Proxy @a)
  translate = translateRTree @(RTreeIsLeaf d) @d @a
  addSubtypes = addSubtypesRTree @(RTreeIsLeaf d) @d @a
  addValue = addValueRTree @(RTreeIsLeaf d) @d @a
  hasLUT = hasLUT @a

class WaveformRTree (isLeaf::Bool) d a where
  addValueRTree :: RTree d a -> LUTMap -> LUTMap
  addSubtypesRTree :: TypeMap -> TypeMap
  translateRTree :: RTree d a -> Translation
instance (Waveform a) => WaveformRTree True 0 a where
  addValueRTree t = if hasLUT @a then
    case t of
      RLeaf x -> addValue x
      _ -> undefined
    else id
  addSubtypesRTree = addTypes @a
  translateRTree t = Translation ren subs
    where
      subs = case t of
        RLeaf x -> [("0",translate x)]
        _ -> undefined
      ren = render (translator @(RTree 0 a)) subs
instance (Waveform (RTree d1 a), Waveform a, d ~ d1 + 1, KnownNat d1) => WaveformRTree False d a where
  addValueRTree t = if hasLUT @a then
    case t of
      RBranch x y -> addValue (x:: RTree d1 a) . addValue y
      _ -> undefined
    else id
  addSubtypesRTree = addTypes @a
  translateRTree t = Translation ren subs
    where
      subs = case t of
        RBranch x y -> [("left",translate (x::RTree d1 a)),("right",translate y)]
        _ -> undefined
      ren = render (translator @(RTree 0 a)) subs
