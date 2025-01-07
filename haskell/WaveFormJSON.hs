
module WaveFormJSON(ToJSON(toJSON)) where

import WaveFormViewer
import WaveFormColor (Color(RGB))

import Text.Printf
import Data.Map (Map,toList)
import qualified Data.Map as Map


structJSON :: [(String,String)] -> String
structJSON xs = "{" ++a0++ foldr (\a b -> ',':a++b) "}" rest
    where (a0:rest) = map (\(k,v) -> printf "%s:%s" (show k) v) xs

enumJSON :: (ToJSON a) => String -> a -> String
enumJSON variant values = structJSON [(variant,toJSON values)]

class ToJSON a where
    toJSON :: a -> String



instance ToJSON String where
    toJSON = show


instance ToJSON TranslationResult where
    toJSON (TranslationResult repr kind sub) = structJSON [("val",toJSON repr),("kind",toJSON kind),("subfields",toJSON sub)]

instance ToJSON ValueRepr where
    toJSON (VRBit c)    = enumJSON "Bit" (c:"")
    toJSON (VRBits s)   = enumJSON "Bits" s
    toJSON (VRString s) = enumJSON "String" s
    toJSON VRNotPresent = show "NotPresent"

instance ToJSON ValueKind where
    toJSON VKNormal     = show "Normal"
    toJSON VKUndef      = show "Undef"
    toJSON VKHighImp    = show "HighImp"
    toJSON VKWarn       = show "Warn"
    toJSON VKDontCare   = show "DontCare"
    toJSON VKWeak       = show "Weak"
    toJSON (VKCustom c) = enumJSON "Custom" c

instance ToJSON Color where
    toJSON (RGB r g b) = printf "[%d,%d,%d,255]" r g b

instance ToJSON [SubFieldTranslationResult] where
    toJSON [] = "[]"
    toJSON xs = "[" ++p++ foldr (\a b -> ',':a++b) "]" ps
        where (p:ps) = map toJSON xs

instance ToJSON [(String,VariableInfo)] where
    toJSON [] = "[]"
    toJSON xs = "[" ++ p ++ foldr (\a b -> ',':a++b) "]" ps
        where (p:ps) = map toJSON xs

instance ToJSON SubFieldTranslationResult where
    toJSON (SubFieldTranslationResult name res) = structJSON [("name",show name), ("result",toJSON res)]

instance ToJSON VariableInfo where

    toJSON (VICompound xs) = structJSON [("Compound",structJSON [("subfields",toJSON xs)])]
    toJSON VIBits   = show "Bits"
    toJSON VIBool   = show "Bool"
    toJSON VIClock  = show "Clock"
    toJSON VIString = show "String"
    toJSON VIReal   = show "Real"

instance ToJSON (String,VariableInfo) where
    toJSON (name,info) = "["++show name++","++toJSON info++"]"

instance (ToJSON a) => ToJSON (VariableInfo,a) where
    toJSON (info,x) = "["++toJSON info++","++toJSON x++"]"

instance (ToJSON a) => ToJSON (Map String a) where
    toJSON mp = structJSON $ map (\(k,v) -> (k,toJSON v)) $ toList mp

