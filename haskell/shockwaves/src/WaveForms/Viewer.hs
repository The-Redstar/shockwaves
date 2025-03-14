
{-# LANGUAGE AllowAmbiguousTypes #-} -- with :set -XAllow....
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-} -- to derive Int etc
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module WaveForms.Viewer (
    Display(..),
    Split(..),
    AutoSplit,
    ValueRepr(..),
    ValueKind(..),
    VariableInfo(..),
    TranslationResult(..),SubFieldTranslationResult(..),
) where
-- import qualified WaveFormViewer as WFV

import Prelude
import GHC.Generics
import GHC.TypeLits
import Data.Proxy

import WaveForms.Color (Color)

-- for instances of standard types:
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector (Vec(..),toList)

-- VALUE REPRESENTATION TYPES
-- | Representation of a value.
data ValueRepr = VRBit Char | VRBits String | VRString String | VRNotPresent deriving Show
-- | Determines the way values are displayed. For most signals, this only determines the color,
-- | but VIBool signals can have lines at different heights.
data ValueKind = VKNormal | VKUndef | VKHighImp | VKCustom Color | VKWarn | VKDontCare | VKWeak deriving Show

-- | Information about the signal structure. The structure presented must match that of the `TranslationResult`s
-- | of the value.
-- | `VICompound` is the only variant allowed to have subsignals.
-- | `VIBool` and `VIClock` are displayed differently in the waveform viewer.
data VariableInfo = VICompound [(String,VariableInfo)] | VIBits | VIBool | VIClock | VIString | VIReal deriving Show

-- | A value representation similar to that used in Surfer. The structure must match that of `VariableInfo`.
data TranslationResult = TranslationResult (ValueRepr,ValueKind) [SubFieldTranslationResult] deriving Show
-- | A wrapper for naming compound signals.
data SubFieldTranslationResult = SubFieldTranslationResult String TranslationResult deriving Show


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


-- | Class for determining the complete translation of a value, including how it is split up.
-- | The structure can be automatically deduced for types implementing `Generic` with all subtypes implementing `Split` as well.
class (Display a) => Split a where
    -- | Translate a value into waveform viewer data.
    translate :: a -> TranslationResult
    default translate :: forall x. (Generic a, AutoSplit (Rep a x), Display a) => a -> TranslationResult
    translate v = autoTranslate (from  v::(Rep a x)) (display v)

    -- | Generate waveform viewer data for when the current value does not exist. Uses `VRNotPresent` for all values.
    notPresent :: TranslationResult
    default notPresent :: forall x. (Generic a, AutoSplit (Rep a x)) => TranslationResult
    notPresent = autoNotPresent @(Rep a x)

    -- | Signal structure of the type.
    structure :: VariableInfo
    structure = translationToStructure $ notPresent @a

-- | Turns a translation into a structural description. Can be used in conjunction
-- | with `notPresent` to determine the structure of a type.
translationToStructure :: TranslationResult -> VariableInfo
translationToStructure (TranslationResult _ [])  = VIString
translationToStructure (TranslationResult _ sub) = VICompound $ map go sub
    where go (SubFieldTranslationResult name res) = (name, translationToStructure res)





-- AUTOMATIC SPLITTING

-- | Helper class for autmatically deriving default `Split` instances using `Generic`.
class AutoSplit a where
    autoTranslate :: a -> (ValueRepr,ValueKind) -> TranslationResult
    autoNotPresent :: TranslationResult

-- instance forall a x. (Display a, Generic a, AutoSplit (Rep a x)) => AutoSplit a where
    -- autoTranslate v _ = autoTranslate (from v) (display v)
    -- autoNotPresent = autoNotPresent @(Rep a x)

instance (AutoSplitConstrs (constrs p)) => AutoSplit (D1 meta constrs p) where
    autoTranslate (M1 x) rk = res
        where
            res = case autoTranslateConstrs @(constrs p) x rk of
                [SubFieldTranslationResult _ constr] -> constr --if there is only one variant, directly move into contained value
                constrs                              -> TranslationResult rk constrs

    autoNotPresent = res --TranslationResult VRNotPresent VKNormal (autoNotPresentConstrs 0 @consts)
        where
            res = case autoNotPresentConstrs @(constrs p) of
                [SubFieldTranslationResult _ constr] -> constr --if there is only one variant, directly move into contained value
                constrs                              -> TranslationResult (VRNotPresent,VKNormal) constrs

class AutoSplitConstrs a where
    autoTranslateConstrs :: a -> (ValueRepr,ValueKind) -> [SubFieldTranslationResult]
    autoNotPresentConstrs :: [SubFieldTranslationResult]

instance (AutoSplitConstrs (a p), AutoSplitConstrs (b p)) => AutoSplitConstrs ((a :+: b) p) where
    autoTranslateConstrs (L1 x) rk = autoTranslateConstrs  @(a p) x rk ++ autoNotPresentConstrs @(b p)
    autoTranslateConstrs (R1 x) rk = autoNotPresentConstrs @(a p)      ++ autoTranslateConstrs  @(b p) x rk

    -- do not include N/A constructors to save on space
    --autoTranslateConstrs (L1 x) rk = autoTranslateConstrs  @(a p) x rk
    --autoTranslateConstrs (R1 x) rk = autoTranslateConstrs  @(b p) x rk

    autoNotPresentConstrs = autoNotPresentConstrs @(a p) ++ autoNotPresentConstrs @(b p)

instance (KnownSymbol name,AutoSplitFields (fields p)) => AutoSplitConstrs (C1 (MetaCons name x y) fields p) where
    autoTranslateConstrs (M1 x) rk = [SubFieldTranslationResult (symbolVal (Proxy @name)) $ TranslationResult rk fields]
        where
            (fields,_) = autoTranslateFields x 0

    autoNotPresentConstrs = [SubFieldTranslationResult (symbolVal (Proxy @name)) $ TranslationResult (VRNotPresent,VKNormal) fields]
        where
            (fields,_) = autoNotPresentFields @(fields p) 0

class AutoSplitFields a where
    autoTranslateFields :: a -> Int -> ([SubFieldTranslationResult],Int)
    autoNotPresentFields :: Int -> ([SubFieldTranslationResult],Int)

instance (AutoSplitFields (a p), AutoSplitFields (b p)) => AutoSplitFields ((a :*: b) p) where
    autoTranslateFields (x :*: y) n = (fieldsa ++ fieldsb, nb)
        where
            (fieldsa,na) = autoTranslateFields x n
            (fieldsb,nb) = autoTranslateFields y na

    autoNotPresentFields n = (fieldsa ++ fieldsb, nb)
        where
            (fieldsa,na) = autoNotPresentFields @(a p) n
            (fieldsb,nb) = autoNotPresentFields @(b p) na

instance (KnownSymbol name,Split a) => AutoSplitFields (S1 (MetaSel (Just name) x y z) (Rec0 a) p) where
    autoTranslateFields M1{unM1=K1{unK1=x}} n = ([SubFieldTranslationResult (symbolVal $ Proxy @name) fieldtrans], n+1)
        where fieldtrans = translate x
    autoNotPresentFields n = ([SubFieldTranslationResult (symbolVal $ Proxy @name) (notPresent @a)], n+1)
instance (                 Split a) => AutoSplitFields (S1 (MetaSel Nothing x y z) (Rec0 a) p) where
    autoTranslateFields M1{unM1=K1{unK1=x}} n = ([SubFieldTranslationResult (show n                 ) fieldtrans], n+1)
        where fieldtrans = translate x
    autoNotPresentFields n = ([SubFieldTranslationResult (show n                 ) (notPresent @a)], n+1)

instance AutoSplitFields (U1 p) where
    autoTranslateFields _ n = ([],n)
    autoNotPresentFields n = ([],n)


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

instance (Show a0,Split a0,Show a1,Split a1) => Split (a0,a1)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2) => Split (a0,a1,a2)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3) => Split (a0,a1,a2,a3)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4) => Split (a0,a1,a2,a3,a4)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5) => Split (a0,a1,a2,a3,a4,a5)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6) => Split (a0,a1,a2,a3,a4,a5,a6)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7) => Split (a0,a1,a2,a3,a4,a5,a6,a7)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9,Show a10,Split a10) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9,Show a10,Split a10,Show a11,Split a11) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9,Show a10,Split a10,Show a11,Split a11,Show a12,Split a12) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9,Show a10,Split a10,Show a11,Split a11,Show a12,Split a12,Show a13,Split a13) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
instance (Show a0,Split a0,Show a1,Split a1,Show a2,Split a2,Show a3,Split a3,Show a4,Split a4,Show a5,Split a5,Show a6,Split a6,Show a7,Split a7,Show a8,Split a8,Show a9,Split a9,Show a10,Split a10,Show a11,Split a11,Show a12,Split a12,Show a13,Split a13,Show a14,Split a14) => Split (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)

-- INSTANCES FOR CLASH TYPES

instance Display (Signed n) where
instance Split (Signed n) where
    translate x = TranslationResult (display x) []
    notPresent = TranslationResult (VRNotPresent,VKNormal) []

instance Display (Unsigned n)
instance Split (Unsigned n) where
    translate x = TranslationResult (display x) []
    notPresent = TranslationResult (VRNotPresent,VKNormal) []

instance (Show a) => Display (Vec n a)
instance (KnownNat n, Split a, Show a) => Split (Vec n a) where
    translate v = TranslationResult (display v) subs
        where
            subs = zipWith (\i s -> SubFieldTranslationResult (show i) (translate s)) [(0::Integer)..] (toList v)

    notPresent = TranslationResult (VRNotPresent,VKNormal) subs
        where
            subs = map (\i -> SubFieldTranslationResult (show i) (notPresent @a)) [0..(natVal $ Proxy @n)]