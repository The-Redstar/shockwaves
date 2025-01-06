
{-# LANGUAGE AllowAmbiguousTypes #-} -- with :set -XAllow....
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-} -- to derive Int etc

module WaveFormViewer (
    Display(..),
    Split(..),
    AutoSplit,
    ValueRepr(..),
    ValueKind(..),
    VariableInfo(..),
    TranslationResult(..),SubFieldTranslationResult(..),
    Color(..),
    red,yellow,green,cyan,blue,magenta,white,gray,
) where
-- import qualified WaveFormViewer as WFV

import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.Data
import Data.List.Split


data ValueRepr = VRBit Char | VRBits String | VRString String | VRNotPresent deriving Show
data ValueKind = VKNormal | VKUndef | VKHighImp | VKCustom Color | VKWarn | VKDontCare | VKWeak deriving Show

data VariableInfo = VICompound [(String,VariableInfo)] | VIBits | VIBool | VIClock | VIString | VIReal deriving Show

data TranslationResult = TranslationResult ValueRepr ValueKind [SubFieldTranslationResult] deriving Show
data SubFieldTranslationResult = SubFieldTranslationResult String TranslationResult deriving Show

data Color = RGB Int Int Int deriving Show

red     = RGB 255 0 0
yellow  = RGB 255 255 0
green   = RGB 0 255 0
cyan    = RGB 0 255 255
blue    = RGB 0 0 255
magenta = RGB 255 0 255
white   = RGB 255 255 255
gray    = RGB 127 127 127


class Display a where
    display :: a -> (ValueRepr,ValueKind)
    display x = (repr x, kind x)

    repr :: a -> ValueRepr
    default repr :: ( Show a ) => a -> ValueRepr
    repr x = VRString $ take 100 $ show x

    kind :: a -> ValueKind
    kind _ = VKNormal

class (Display a) => Split a where
    translate :: a -> TranslationResult
    default translate :: forall x. (Generic a, AutoSplit (Rep a x), Display a) => a -> TranslationResult
    translate v = autoTranslate ((from  v)::(Rep a x)) (display v)

    notPresent :: TranslationResult
    default notPresent :: forall x. (Generic a, AutoSplit (Rep a x)) => TranslationResult
    notPresent = autoNotPresent @(Rep a x)

    structure :: VariableInfo
    structure = notPresentToStructure $ notPresent @a


--decode :: (BitPack a) => BitVector -> TranslationResult
--decode bv = translate $ unpack bv

notPresentToStructure (TranslationResult _ _ [])  = VIString
notPresentToStructure (TranslationResult _ _ sub) = VICompound $ map (\(SubFieldTranslationResult name res) -> (name, notPresentToStructure res)) sub








class AutoSplit a where
    autoTranslate :: a -> (ValueRepr,ValueKind) -> TranslationResult
    autoNotPresent :: TranslationResult

-- instance forall a x. (Display a, Generic a, AutoSplit (Rep a x)) => AutoSplit a where
    -- autoTranslate v _ = autoTranslate (from v) (display v)
    -- autoNotPresent = autoNotPresent @(Rep a x)

instance (AutoSplitConstrs (constrs p)) => AutoSplit (D1 meta constrs p) where
    autoTranslate (M1 x) (repr,kind) = res
        where
            res = case autoTranslateConstrs @(constrs p) x (repr,kind) of
                [SubFieldTranslationResult _ constr] -> constr --if there is only one variant, directly move into contained value
                constrs                              -> TranslationResult repr kind constrs

    autoNotPresent = res --TranslationResult VRNotPresent VKNormal (autoNotPresentConstrs 0 @consts)
        where
            res = case autoNotPresentConstrs @(constrs p) of
                [SubFieldTranslationResult _ constr] -> constr --if there is only one variant, directly move into contained value
                constrs                              -> TranslationResult VRNotPresent VKNormal constrs

class AutoSplitConstrs a where
    autoTranslateConstrs :: a -> (ValueRepr,ValueKind) -> [SubFieldTranslationResult]
    autoNotPresentConstrs :: [SubFieldTranslationResult]

instance (AutoSplitConstrs (a p), AutoSplitConstrs (b p)) => AutoSplitConstrs ((a :+: b) p) where
    autoTranslateConstrs (L1 x) rk = autoTranslateConstrs  @(a p) x rk ++ autoNotPresentConstrs @(b p)
    autoTranslateConstrs (R1 x) rk = autoNotPresentConstrs @(a p)      ++ autoTranslateConstrs  @(b p) x rk

    -- do not include N/A constructors to svae on space
    --autoTranslateConstrs (L1 x) rk = autoTranslateConstrs  @(a p) x rk
    --autoTranslateConstrs (R1 x) rk = autoTranslateConstrs  @(b p) x rk

    autoNotPresentConstrs = autoNotPresentConstrs @(a p) ++ autoNotPresentConstrs @(b p)

instance (KnownSymbol name,AutoSplitFields (fields p)) => AutoSplitConstrs (C1 (MetaCons name x y) fields p) where
    autoTranslateConstrs (M1 x) (repr,kind) = [SubFieldTranslationResult (symbolVal (Proxy @name)) $ TranslationResult repr kind fields]
        where
            (fields,_) = autoTranslateFields x 0

    autoNotPresentConstrs = [SubFieldTranslationResult (symbolVal (Proxy @name)) $ TranslationResult VRNotPresent VKNormal fields]
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
