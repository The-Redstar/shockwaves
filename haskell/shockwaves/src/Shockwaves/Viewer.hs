
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-} -- to derive Maybe etc
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Shockwaves.Viewer (
    Display(..),
    Split(..),
    AutoSplit(..),
    ValueRepr(..),
    ValueKind(..),
    VariableInfo(..),
    TranslationResult(..),SubFieldTranslationResult(..),
    translate, safeDisplay, safeSplit,
    NoSplit(..), DisplayX(..),
) where
-- import qualified WaveFormViewer as WFV

import Prelude
import GHC.Generics
import GHC.TypeLits
import Data.Proxy

import Shockwaves.Color      (Color)

-- undefined handling
-- import System.IO.Unsafe     (unsafeDupablePerformIO)
-- import Control.Exception    (catch,evaluate)
import Control.DeepSeq      (NFData, force)
import Clash.XException     (NFDataX, isX, ShowX(..))
import GHC.IO.Unsafe        (unsafeDupablePerformIO)
import Control.Exception    (evaluate,catch,SomeException)
import Data.Maybe           (fromMaybe)

-- for instances of standard types:
import Clash.Sized.Signed   (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector   (Vec(..),toList)
import Clash.Prelude        (BitVector, Bit, Index, bv2v, RTree(..), SNat)
import Data.Int             (Int8,Int16,Int32,Int64)
import Data.Word            (Word8,Word16,Word32,Word64)
-- import Numeric.Half.Internal (Half)
import Foreign.C            (CUShort)
import Data.Complex         (Complex)
import Data.Ord             (Down)
import Data.Functor.Identity(Identity)
import Clash.Num.Zeroing    (Zeroing, fromZeroing)
import Clash.Num.Wrapping   (Wrapping, fromWrapping)
import Clash.Num.Saturating (Saturating, fromSaturating)
import Clash.Num.Overflowing(Overflowing, fromOverflowing)
import Clash.Num.Erroring   (Erroring, fromErroring)
import Clash.Sized.Fixed    (Fixed)
import Data.Functor.Const   (Const)
import Data.Functor.Product (Product)
import Data.Functor.Classes (Show1)
import Data.Functor.Sum (Sum)
import Data.Functor.Compose (Compose)

-- VALUE REPRESENTATION TYPES

-- | Representation of a value.
data ValueRepr
    = VRBit Char      -- ^ Single bit
    | VRBits String   -- ^ Bitvector
    | VRString String -- ^ Text
    | VRNotPresent    -- ^ Signal not present; do not display signal at all
  deriving (Show,Generic,NFData,NFDataX)

-- | Determines the way values are displayed. For most signals, this only determines the color,
-- but VIBool signals can have lines at different heights for the different value types.
data ValueKind
    = VKNormal        -- ^ Green
    | VKUndef         -- ^ Red
    | VKHighImp       -- ^ Yellow
    | VKCustom Color  -- ^ Any
    | VKWarn          -- ^ Red
    | VKDontCare      -- ^ Blue
    | VKWeak          -- ^ Gray
  deriving (Show,Generic,NFData,NFDataX)

-- | Information about the signal structure.
-- `VICompound` is the only variant allowed to have subsignals. Translation results must match this structure.
-- `VIBool` and `VIClock` are displayed differently in the waveform viewer (as waves).
data VariableInfo = VICompound [(String,VariableInfo)] | VIBits | VIBool | VIClock | VIString | VIReal deriving Show

-- | A value representation similar to that used in Surfer. The structure must match that of `VariableInfo`.
data TranslationResult = TranslationResult (ValueRepr,ValueKind) [SubFieldTranslationResult] deriving (Show,Generic,NFData,NFDataX)

-- | A wrapper for naming compound signals.
data SubFieldTranslationResult = SubFieldTranslationResult String TranslationResult deriving (Show,Generic,NFData,NFDataX)

type TR = TranslationResult
type STR = SubFieldTranslationResult

tr :: (ValueRepr,ValueKind) -> [STR] -> TR
tr = TranslationResult
str :: String -> TR -> STR
str = SubFieldTranslationResult



-- MAIN CLASSES

-- | Class determining the appearance of a value in the waveform viewer (text and color).
-- By default, this uses `Show` and `VKNormal`.
class Display a where
    display :: a -> (ValueRepr,ValueKind)
    display x = (repr x, kind x)

#ifndef DEFAULT_SHOWX
    repr :: a -> ValueRepr
    default repr :: ( Show a ) => a -> ValueRepr
    repr x = VRString $ take 100 $ show x

    kind :: a -> ValueKind
    kind _ = VKNormal
#else
    repr :: a -> ValueRepr
    default repr :: (ShowX a, Display (DisplayX a)) => a -> ValueRepr
    repr x = repr (DisplayX x)

    kind :: a -> ValueKind
    default kind :: (Display (DisplayX a)) => a -> ValueKind
    kind x = kind (DisplayX x)
#endif

newtype DisplayX a = DisplayX a deriving (Generic,ShowX)
instance (ShowX a) => Display (DisplayX a) where
    repr (DisplayX x) = VRString $ take 100 $ showX x
    kind (DisplayX x) = case isX x of
        Right _ -> VKNormal
        Left  _ -> VKUndef

-- | Display a value if properly defined, else return `Nothing`.
safeDisplay :: (Display a) => a -> Maybe (ValueRepr,ValueKind)
safeDisplay x = unsafeDupablePerformIO (catch (evaluate . force . Just $ display x)
                                       (\(_::SomeException) -> return Nothing))

-- | Class for determining the structure and value of subsignals.
-- The structure can be automatically deduced for types implementing `Generic` with all subtypes implementing `Split` as well.
class Split a where
    -- | The structure for the signal. Only VICompound types allow for subsignals.
    -- In addition the the structure of subsignals, this also controls the way the current signal is displayed.AutoSplit
    -- Most data will be shown as blocks, but boolean types are displayed as a single line that can be high or low (or in between for special values).
    structure :: VariableInfo
    default structure :: forall x. (Generic a, AutoSplit (Rep a x)) => VariableInfo
    structure = autoStructure @(Rep a x)

    -- | Function to create the data for subsignals of a given type. If `structure` is not of the `VICompound` type, this list must be empty.AutoSplit
    -- Subsignals need to share the names and order used in `structure`, but not all subsignals need to be provided.AutoSplit
    -- Subsignals that are left out will be automatically set to `VRNotPresent`.
    -- The function is given a copy of the display value, in case this needs to be copied.
    split :: a -> (ValueRepr,ValueKind) -> [STR]
    default split :: forall x. (Generic a, AutoSplit (Rep a x)) => a -> (ValueRepr,ValueKind) -> [STR]
    split x = autoSplit (from @a @x x) --(maybe (VRString "undefined",VKUndef) id $ safeDisplay x)

newtype NoSplit a = NoSplit a
instance Split (NoSplit a) where
    structure = VIString
    split _ _ = []

-- | Split a value into values for its subsignals if properly defined, else return `Nothing`.
safeSplit :: (Split a) => a -> (ValueRepr,ValueKind) -> [STR]
safeSplit x rk = unsafeDupablePerformIO (catch (evaluate . force $ split x rk)
                                        (\(_::SomeException) -> return []))
    -- where go a = unsafeDupablePerformIO (catch (evaluate . force $ Just a)
    --                                     (\(XException _) -> return Nothing))

-- | Translate a value using `safeDisplay` and `safeSplit`. If either returns `Nothing`, return a translation representing undefined instead.
translate :: (Display a, Split a) => a -> TranslationResult
translate x = tr rk $ safeSplit x rk
  where rk = fromMaybe (VRString "undefined", VKUndef) (safeDisplay x)



-- AUTOMATIC SPLITTING

-- | Helper class for automatically deriving default `Split` instances using `Generic`.
class AutoSplit a where
    -- | Like `Split`'s `structure`
    autoStructure :: VariableInfo
    -- | Like `Split`'s `split`
    autoSplit :: a -> (ValueRepr,ValueKind) -> [STR]

instance (AutoSplitConstrs ((a :+: b) p)) => AutoSplit (D1 meta (a :+: b) p) where -- multiple constructors
    autoStructure = case autoStructureConstrs @((a :+: b) p) of
        [] -> VIString
        sub -> VICompound sub
    autoSplit (M1 x) rk = autoSplitConstrs @((a :+: b) p) x rk

instance (KnownSymbol name,AutoSplitFields (fields p)) => AutoSplit (D1 meta (C1 (MetaCons name x y) fields) p) where -- single constructor
    autoStructure = autoStructureFields' @(fields p)
    autoSplit (M1 (M1 x)) _rk = autoSplitFields' x

class AutoSplitConstrs a where
    autoStructureConstrs :: [(String,VariableInfo)]
    autoSplitConstrs :: a -> (ValueRepr,ValueKind) -> [STR] -- should always have length 1

instance (AutoSplitConstrs (a p), AutoSplitConstrs (b p)) => AutoSplitConstrs ((a :+: b) p) where
    autoSplitConstrs (L1 x) rk = autoSplitConstrs  @(a p) x rk
    autoSplitConstrs (R1 x) rk = autoSplitConstrs  @(b p) x rk

    autoStructureConstrs = autoStructureConstrs @(a p) ++ autoStructureConstrs @(b p)

instance (KnownSymbol name,AutoSplitFields (fields p)) => AutoSplitConstrs (C1 (MetaCons name x y) fields p) where
    autoStructureConstrs = [(symbolVal (Proxy @name), autoStructureFields' @(fields p))]
    autoSplitConstrs (M1 x) rk = [str (symbolVal (Proxy @name)) $ tr rk fields]
        where fields = autoSplitFields' x

class AutoSplitFields a where
    autoStructureFields :: Int -> ([(String,VariableInfo)],Int) -- int for assigning numbers to signal names
    autoSplitFields :: a -> Int -> ([STR],Int)

    autoStructureFields' :: VariableInfo -- start at 0 and turn into VariableInfo
    autoStructureFields' = noEmptyCompound . VICompound . fst $ autoStructureFields @a 0

    autoSplitFields' :: a -> [STR] -- start at 0
    autoSplitFields' x = fst $ autoSplitFields x 0


instance (AutoSplitFields (a p), AutoSplitFields (b p)) => AutoSplitFields ((a :*: b) p) where
    autoStructureFields n = (fieldsa ++ fieldsb, nb)
        where
            (fieldsa,na) = autoStructureFields @(a p) n
            (fieldsb,nb) = autoStructureFields @(b p) na

    autoSplitFields (x :*: y) n = (fieldsa ++ fieldsb, nb)
        where
            (fieldsa,na) = autoSplitFields x n
            (fieldsb,nb) = autoSplitFields y na


instance (KnownSymbol name,Split a,Display a) => AutoSplitFields (S1 (MetaSel (Just name) x y z) (Rec0 a) p) where
    autoStructureFields n =                 ([    (symbolVal $ Proxy @name, structure @a)], n+1)
    autoSplitFields M1{unM1=K1{unK1=x}} n = ([str (symbolVal $ Proxy @name) (translate x)], n+1)

instance (                 Split a,Display a) => AutoSplitFields (S1 (MetaSel Nothing x y z) (Rec0 a) p) where
    autoStructureFields n =                 ([    (show n                 , structure @a)], n+1)
    autoSplitFields M1{unM1=K1{unK1=x}} n = ([str (show n                 ) (translate x)], n+1)

instance AutoSplitFields (U1 p) where
    autoSplitFields _ n = ([],n)
    autoStructureFields n = ([],n)

noEmptyCompound :: VariableInfo -> VariableInfo
noEmptyCompound (VICompound []) = VIString
noEmptyCompound vi = vi


-- INSTANCES FOR TUPLES

#ifndef DEFAULT_SHOWX
instance (Show a0,Show a1) => Display (a0,a1)
instance (Show a0,Show a1,Show a2) => Display (a0,a1,a2)
instance (Show a0,Show a1,Show a2,Show a3) => Display (a0,a1,a2,a3)
instance (Show a0,Show a1,Show a2,Show a3,Show a4) => Display (a0,a1,a2,a3,a4)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5) => Display (a0,a1,a2,a3,a4,a5)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6) => Display (a0,a1,a2,a3,a4,a5,a6)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7) => Display (a0,a1,a2,a3,a4,a5,a6,a7)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9,Show a10) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9,Show a10,Show a11) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9,Show a10,Show a11,Show a12) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9,Show a10,Show a11,Show a12,Show a13) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
instance (Show a0,Show a1,Show a2,Show a3,Show a4,Show a5,Show a6,Show a7,Show a8,Show a9,Show a10,Show a11,Show a12,Show a13,Show a14) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)
#else
instance (ShowX a0,ShowX a1) => Display (a0,a1)
instance (ShowX a0,ShowX a1,ShowX a2) => Display (a0,a1,a2)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3) => Display (a0,a1,a2,a3)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4) => Display (a0,a1,a2,a3,a4)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5) => Display (a0,a1,a2,a3,a4,a5)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6) => Display (a0,a1,a2,a3,a4,a5,a6)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7) => Display (a0,a1,a2,a3,a4,a5,a6,a7)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9,ShowX a10) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9,ShowX a10,ShowX a11) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
--instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9,ShowX a10,ShowX a11,ShowX a12) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
--instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9,ShowX a10,ShowX a11,ShowX a12,ShowX a13) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
--instance (ShowX a0,ShowX a1,ShowX a2,ShowX a3,ShowX a4,ShowX a5,ShowX a6,ShowX a7,ShowX a8,ShowX a9,ShowX a10,ShowX a11,ShowX a12,ShowX a13,ShowX a14) => Display (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)
#endif

instance (Display a0,Split a0,Display a1,Split a1) => Split (a0,a1)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2) => Split (a0,a1,a2)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3) => Split (a0,a1,a2,a3)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4) => Split (a0,a1,a2,a3,a4)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5) => Split (a0,a1,a2,a3,a4,a5)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6) => Split (a0,a1,a2,a3,a4,a5,a6)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7) => Split (a0,a1,a2,a3,a4,a5,a6,a7)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9,Display a10,Split a10) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9,Display a10,Split a10,Display a11,Split a11) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9,Display a10,Split a10,Display a11,Split a11,Display a12,Split a12) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9,Display a10,Split a10,Display a11,Split a11,Display a12,Split a12,Display a13,Split a13) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
instance (Display a0,Split a0,Display a1,Split a1,Display a2,Split a2,Display a3,Split a3,Display a4,Split a4,Display a5,Split a5,Display a6,Split a6,Display a7,Split a7,Display a8,Split a8,Display a9,Split a9,Display a10,Split a10,Display a11,Split a11,Display a12,Split a12,Display a13,Split a13,Display a14,Split a14) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)

-- INSTANCES FOR OTHER STANDARD HASKELL TYPES

instance Display ()
deriving via NoSplit () instance Split ()

instance Display Bool where
    repr x = VRBit (if x then '1' else '0')
instance Split Bool where
    structure = VIBool
    split _ _ = []

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Maybe a) where
#else
instance (ShowX a) => Display (Maybe a) where
#endif
#ifdef NOTHING_DC
    kind Nothing = VKDontCare
#else
    kind Nothing = VKNormal
#endif
    kind _ = VKNormal
instance (Display a, Split a) => Split (Maybe a) where
    structure = VICompound [("Just.0",structure @a)]

    split Nothing _ = []
    split (Just y) _ = [str "Just.0" $ translate y]

#ifndef DEFAULT_SHOWX
instance (Show a, Show b) => Display (Either a b) where
#else
instance (ShowX a, ShowX b) => Display (Either a b) where
#endif
#ifdef LEFT_ERR
    kind (Left _) = VKWarn
#endif
    kind _ = VKNormal
instance (Display a, Split a, Display b, Split b) => Split (Either a b)


instance Display Char
deriving via NoSplit Char instance Split Char


instance Display Bit
instance Split Bit where
    structure = VIBool
    split _ _ = []

instance Display Double where
deriving via NoSplit Double instance Split Double

instance Display Float where
deriving via NoSplit Float instance Split Float

instance Display Int where
deriving via NoSplit Int instance Split Int

instance Display Int8 where
deriving via NoSplit Int8 instance Split Int8

instance Display Int16 where
deriving via NoSplit Int16 instance Split Int16

instance Display Int32 where
deriving via NoSplit Int32 instance Split Int32

instance Display Int64 where
deriving via NoSplit Int64 instance Split Int64

instance Display Ordering where
deriving via NoSplit Ordering instance Split Ordering

instance Display Word where
deriving via NoSplit Word instance Split Word

instance Display Word8 where
deriving via NoSplit Word8 instance Split Word8

instance Display Word16 where
deriving via NoSplit Word16 instance Split Word16

instance Display Word32 where
deriving via NoSplit Word32 instance Split Word32

instance Display Word64 where
deriving via NoSplit Word64 instance Split Word64

instance Display CUShort where
deriving via NoSplit CUShort instance Split CUShort

-- instance Display Half where
-- deriving via NoSplit Half instance Split Half

instance Display (Signed n) where
deriving via NoSplit (Signed n) instance Split (Signed n)

instance Display (Unsigned n)
deriving via NoSplit (Unsigned n) instance Split (Unsigned n)

instance Display (Index n)
deriving via NoSplit (Index n) instance Split (Index n)

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Complex a)
#else
instance (ShowX a) => Display (Complex a)
#endif
instance (Display a, Split a) => Split (Complex a)

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Down a)
#else
instance (ShowX a) => Display (Down a)
#endif
instance (Display a, Split a) => Split (Down a)

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Identity a)
#else
instance (ShowX a) => Display (Identity a)
#endif
instance (Display a, Split a) => Split (Identity a)

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Zeroing a)
#else
instance (ShowX a) => Display (Zeroing a)
#endif
instance (Display a, Split a) => Split (Zeroing a) where
    structure = VICompound [("0",structure @a)]
    split z _ = [str "0" $ translate $ fromZeroing z]

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Wrapping a)
#else
instance (ShowX a) => Display (Wrapping a)
#endif
instance (Display a, Split a) => Split (Wrapping a) where
    structure = VICompound [("0",structure @a)]
    split z _ = [str "0" $ translate $ fromWrapping z]

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Saturating a)
#else
instance (ShowX a) => Display (Saturating a)
#endif
instance (Display a, Split a) => Split (Saturating a) where
    structure = VICompound [("0",structure @a)]
    split z _ = [str "0" $ translate $ fromSaturating z]

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Overflowing a)
#else
instance (ShowX a) => Display (Overflowing a)
#endif
instance (Display a, Split a) => Split (Overflowing a) where
    structure = VICompound [("0",structure @a)]
    split z _ = [str "0" $ translate $ fromOverflowing z]

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Erroring a)
#else
instance (ShowX a) => Display (Erroring a)
#endif
instance (Display a, Split a) => Split (Erroring a) where
    structure = VICompound [("0",structure @a)]
    split z _ = [str "0" $ translate $ fromErroring z]

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Vec n a)
#else
instance (ShowX a) => Display (Vec n a)
#endif
instance (KnownNat n, Split a, Display a) => Split (Vec n a) where
    structure = VICompound $ map (\i -> (show i,structure @a)) [0..(natVal $ Proxy @n)]

    split v _ = zipWith (\i s -> SubFieldTranslationResult (show i) (translate s)) [(0::Integer)..] (toList v)

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (RTree d a)
instance (Show a, Display a, Split a, KnownNat d) => Split (RTree d a) where
#else
instance (ShowX a) => Display (RTree d a)
instance (ShowX a, Display a, Split a, KnownNat d) => Split (RTree d a) where
#endif
    structure = structureRTree (natVal $ Proxy @d) (structure @a)
    split (RLeaf x)     _ = [str "0" $ translate x]
    split (RBranch l r) _ = [str "left" $ translate l, str "right" $ translate r]

structureRTree :: Integer -> VariableInfo -> VariableInfo
structureRTree d sa = VICompound $ if d==0 then [("0",sa)] else [("left",tree'),("right",tree')]
    where tree' = structureRTree (d-1) sa


instance (KnownNat n) => Display (BitVector n)
instance (KnownNat n) => Split (BitVector n) where
    structure = structure @(Vec n Bit)
    split = split . bv2v

#ifndef DEFAULT_SHOWX
instance (Show a) => Display (Const a b)
#else
instance (ShowX a) => Display (Const a b)
#endif
instance (Display a, Split a) => Split (Const a b)

#ifndef DEFAULT_SHOWX
instance (Show (Fixed r i f)) => Display (Fixed r i f)
#else
instance (ShowX (Fixed r i f)) => Display (Fixed r i f)
#endif
deriving via NoSplit (Fixed r i f) instance Split (Fixed r i f)

#ifndef DEFAULT_SHOWX
instance (Show1 f, Show1 g, Show a) => Display (Product f g a)
#else
instance (Show1 f, Show1 g, ShowX a) => Display (Product f g a)
#endif
instance (Display (f a), Display (g a), Split (f a), Split (g a)) => Split (Product f g a)

#ifndef DEFAULT_SHOWX
instance (Show1 f, Show1 g, Show a) => Display (Sum f g a)
#else
instance (Show1 f, Show1 g, ShowX a) => Display (Sum f g a)
#endif
instance (Display (f a), Display (g a), Split (f a), Split (g a)) => Split (Sum f g a)

#ifndef DEFAULT_SHOWX
instance (Show1 f, Show1 g, Show a) => Display (Compose f g a)
#else
instance (Show1 f, Show1 g, ShowX a) => Display (Compose f g a)
#endif
instance (Display (f (g a)), Display (g a), Split (f (g a)), Split (g a)) => Split (Compose f g a)

instance Display (Clash.Prelude.SNat n)
deriving via NoSplit (Clash.Prelude.SNat n) instance Split (Clash.Prelude.SNat n)

instance Display (Proxy a)
deriving via NoSplit (Proxy a) instance Split (Proxy a)