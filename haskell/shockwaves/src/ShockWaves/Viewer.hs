
{-# LANGUAGE AllowAmbiguousTypes #-} -- with :set -XAllow....
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-} -- to derive Int etc
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module ShockWaves.Viewer (
    Display(..),
    Split(..),
    AutoSplit(..),
    ValueRepr(..),
    ValueKind(..),
    VariableInfo(..),
    TranslationResult(..),SubFieldTranslationResult(..),
    translate, safeDisplay, safeSplit,
) where
-- import qualified WaveFormViewer as WFV

import Prelude
import GHC.Generics
import GHC.TypeLits
import Data.Proxy

import WaveForms.Color      (Color)

-- undefined handling
-- import System.IO.Unsafe     (unsafeDupablePerformIO)
-- import Control.Exception    (catch,evaluate)
import Control.DeepSeq      (NFData)
import Clash.XException     (NFDataX, hasX)


-- for instances of standard types:
import Clash.Sized.Signed   (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector   (Vec(..),toList)
import Clash.Prelude (BitVector, Bit, Index, bv2v)

-- VALUE REPRESENTATION TYPES

-- | Representation of a value.
data ValueRepr
    = VRBit Char      -- ^ Single bit
    | VRBits String   -- ^ Bitvector
    | VRString String -- ^ Text
    | VRNotPresent    -- ^ Signal not present; do not display signal at all
  deriving (Show,Generic,NFData,NFDataX)

-- | Determines the way values are displayed. For most signals, this only determines the color,
-- | but VIBool signals can have lines at different heights for the different value types.
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
-- | `VICompound` is the only variant allowed to have subsignals. Translation results must match this structure.
-- | `VIBool` and `VIClock` are displayed differently in the waveform viewer (as waves).
data VariableInfo = VICompound [(String,VariableInfo)] | VIBits | VIBool | VIClock | VIString | VIReal deriving Show

-- | A value representation similar to that used in Surfer. The structure must match that of `VariableInfo`.
data TranslationResult = TranslationResult (ValueRepr,ValueKind) [SubFieldTranslationResult] deriving (Show,Generic,NFData,NFDataX)

-- | A wrapper for naming compound signals.
data SubFieldTranslationResult = SubFieldTranslationResult String TranslationResult deriving (Show,Generic,NFData,NFDataX)

type TR = TranslationResult
type STR = SubFieldTranslationResult


-- MAIN CLASSES

-- | Class determining the appearance of a value in the waveform viewer (text and color).
-- | By default, this uses `Show` and `VKNormal`.
class Display a where
    display :: a -> (ValueRepr,ValueKind)
    display x = (repr x, kind x)

    repr :: a -> ValueRepr
    default repr :: ( Show a ) => a -> ValueRepr
    repr x = VRString $ take 100 $ show x

    kind :: a -> ValueKind
    kind _ = VKNormal

-- | Display a value if properly defined, else return `Nothing`.
safeDisplay :: a -> Maybe (ValueRepr,ValueKind)
safeDisplay x = unsafeDupablePerformIO (catch (evaluate . Just . force $ display x)
                                       (\(XException _) -> return None))

-- | Class for determining the structure and value of subsignals.
-- | The structure can be automatically deduced for types implementing `Generic` with all subtypes implementing `Split` as well.
class Split a where
    -- | The structure for the signal. Only VICompound types allow for subsignals.
    -- | In addition the the structure of subsignals, this also controls the way the current signal is displayed.AutoSplit
    -- | Most data will be shown as blocks, but boolean types are displayed as a single line that can be high or low (or in between for special values).
    structure :: VariableInfo
    default structure :: forall x. (Generic a, AutoSplit (Rep a x)) => VariableInfo
    structure = autoStructure @(Rep a x)

    -- | Function to create the data for subsignals of a given type. If `structure` is not of the `VICompound` type, this list must be empty.AutoSplit
    -- | Subsignals need to share the names and order used in `structure`, but not all subsignals need to be provided.AutoSplit
    -- | Subsignals that are left out will be automatically set to `VRNotPresent`.
    split :: a -> [STR]
    default split :: forall x. (Generic a, AutoSplit (Rep a x)) => a -> [STR]
    split x = autoSplit (from x) (maybe (VRString "undefined",VKUndef) id $ safeDisplay x)

newtype NoSplit = NoSplit
instance Split NoSplit where
    structure = VIString
    split _ = []

-- | Split a value into values for its subsignals if properly defined, else return `Nothing`.
safeSplit :: (Split a) => a -> Maybe [STR]
safeSplit x = unsafeDupablePerformIO (catch (evaluate. Just . force $ translate x)
                                     (\(XException _) -> Nothing))

-- | Translate a value using `safeDisplay` and `safeSplit`. If either returns `Nothing`, return a translation representing undefined instead.
translate :: (Display a, Split a) => a -> TranslationResult
translate x = case (safeDisplay x, safeSplit x) of
    (Just d , Just s) => TR d s
    (Nothing, Just s) => TR (VRString "undefined",VKUndef) s
    _                 => TR (VRString "undefined",VKUndef) []



-- AUTOMATIC SPLITTING

-- | Helper class for automatically deriving default `Split` instances using `Generic`.
class AutoSplit a where
    -- | Like `Split`'s `structure`
    autoStructure :: VariableInfo
    -- | Like `Split`'s `split`
    autoSplit :: a -> (ValueRepr,ValueKind) -> TranslationResult

-- instance (AutoSplitConstrs (constrs p)) => AutoSplit (D1 meta constrs p) where
--     autoSplit (M1 x) rk = res
--       where
--         res = case (autoStructure @(constrs p), autoSplit @(constrs p) x rk) of
--             (VICompound [(_,_)], [STR _ (TR _ sub)]) -> sub -- unpack single data constructor compound types
--             (_,tr)                                   -> tr

--     autoStructure = res
--       where
--         res = case autoStructure @(constrs p) of
--             VICompound [(_,vi)] -> vi -- unpack single data constructor compound types
--             vi                  -> vi

instance (AutoSplitConstrs (constrs p)) => AutoSplit (D1 meta constrs@(a :+: b) p) where -- multiple constructors
    autoStructure = autoStructureConstrs @(constrs p)
    autoSplit (M1 x) _ = autoSplitConstrs @(constrs p) x rk

instance (AutoSplitConstrs (constrs p)) => AutoSplit (D1 meta (C1 (MetaCons name x y) fields p) p) where -- single constructor
    autoStructure = [(symbolVal (Proxy @name), autoStructureFields' @(fields p))]
    autoSplit (M1 (M1 x)) rk = [STR (symbolVal (Proxy @name)) $ TR rk fields]
        where fields = autoSplitFields' x

class AutoSplitConstrs a where
    autoStructureConstrs :: [(String,VariableInfo)]
    autoSplitConstrs :: a -> (ValueRepr,ValueKind) -> [STR] -- should always have length 1

instance (AutoSplitConstrs (a p), AutoSplitConstrs (b p)) => AutoSplitConstrs ((a :+: b) p) where
    autoSplitConstrs (L1 x) rk = autoSplitConstrs  @(a p) x rk
    autoSplitConstrs (R1 x) rk = autoSplitConstrs  @(b p) x rk

    autoStructureConstrs = autoStructureConstrs @(a p) ++ autoStructureConstrs @(b p)

instance (KnownSymbol name,AutoSplitFields (fields p)) => AutoSplitConstrs (C1 (MetaCons name x y) fields p) where
    autoStructureConstrs = [(symbolVal (Proxy @name), autoStructureFields' @(fields p))]
    autoSplitConstrs (M1 x) rk = [STR (symbolVal (Proxy @name)) $ TR rk fields]
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


instance (KnownSymbol name,Split a) => AutoSplitFields (S1 (MetaSel (Just name) x y z) (Rec0 a) p) where
    autoStructureFields n =                 ([    (symbolVal $ Proxy @name, structure @a)], n+1)
    autoSplitFields M1{unM1=K1{unK1=x}} n = ([STR (symbolVal $ Proxy @name) (translate x)], n+1)

instance (                 Split a) => AutoSplitFields (S1 (MetaSel Nothing x y z) (Rec0 a) p) where
    autoStructureFields n =                 ([    (show n                 , structure @a)], n+1)
    autoSplitFields M1{unM1=K1{unK1=x}} n = ([STR (show n                 ) (translate x)], n+1)

instance AutoSplitFields (U1 p) where
    autoSplitFields _ n = ([],n)
    autoStructureFields n = ([],n)

noEmptyCompound :: VariableInfo -> VariableInfo
noEmptyCompound (VICompound []) = VIString
noEmptyCompound vi = vi


-- INSTANCES FOR TUPLES
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

instance (Split a0,Split a1) => Split (a0,a1)
instance (Split a0,Split a1,Split a2) => Split (a0,a1,a2)
instance (Split a0,Split a1,Split a2,Split a3) => Split (a0,a1,a2,a3)
instance (Split a0,Split a1,Split a2,Split a3,Split a4) => Split (a0,a1,a2,a3,a4)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5) => Split (a0,a1,a2,a3,a4,a5)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6) => Split (a0,a1,a2,a3,a4,a5,a6)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7) => Split (a0,a1,a2,a3,a4,a5,a6,a7)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9,Split a10) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9,Split a10,Split a11) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9,Split a10,Split a11,Split a12) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9,Split a10,Split a11,Split a12,Split a13) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
instance (Split a0,Split a1,Split a2,Split a3,Split a4,Split a5,Split a6,Split a7,Split a8,Split a9,Split a10,Split a11,Split a12,Split a13,Split a14) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)

-- INSTANCES FOR OTHER STANDARD HASKELL TYPES

instance Display Bool where
    repr x = VRBit (if x then '1' else '0')
deriving Split Bool where
    structure = VIBool
    split _ = []

instance (Show a) => Display (Maybe a)
instance (Show a, Split a) => Split (Maybe a) where
    structure = VICompound [("Just.0",structure @a)]

    split Nothing = []
    split (Just y) = [SR "Just.0" $ translate y]

instance (Show a, Show b) => Display (Either a b)
instance (Show  a, Split a, Show b, Split b) => Split (Either a b)

-- INSTANCES FOR CLASH TYPES

instance Display Bit
instance Split Bit where
    structure = VIBool
    split _ = []

instance Display (Signed n) where
deriving Split (Signed n) via NoSplit

instance Display (Unsigned n)
deriving Split (Unsigned n) via NoSplit

instance Display (Index n)
deriving Split (Index n) via NoSplit

instance (Show a) => Display (Vec n a)
instance (KnownNat n, Split a, Show a) => Split (Vec n a) where
    structure = VICompound $ map (\i -> (show i,structure @a)) [0..(natVal $ Proxy @n)]

    split v = zipWith (\i s -> SubFieldTranslationResult (show i) (translate s)) [(0::Integer)..] (toList v)

instance (KnownNat n) => Display (BitVector n)
instance (KnownNat n) => Split (BitVector n) where
    structure = structure @(Vec n Bit)
    split = split . bv2v