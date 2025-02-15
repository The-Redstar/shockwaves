{-# LANGUAGE FlexibleInstances #-}

module WhockWaves.JSON(ToJSON(toJSON)) where

import Prelude

import ShockWaves.Viewer
import ShockWaves.Color (Color(RGB))

import Text.Printf
import Data.Map (Map,toList)
import Data.Maybe (catMaybes)


structJSON :: [(String,String)] -> String
structJSON xs = go $ map (\(k,v) -> printf "%s:%s" (show k) v) xs
    where go [] = "{}"
          go (a0:rest) = "{" ++a0++ foldr (\a b -> ',':a++b) "}" rest

enumJSON :: (ToJSON a) => String -> a -> String
enumJSON variant values = structJSON [(variant,toJSON values)]


-- | Simple class for turning translation object into JSON.
class ToJSON a where
    toJSON :: a -> String



instance ToJSON String where
    toJSON = show


instance ToJSON TranslationResult where
    toJSON (TranslationResult (r,k) sub) = structJSON $ catMaybes [
        Just ("v",toJSON r),
        case k of
            VKNormal -> Nothing
            _ -> Just ("k",toJSON k),
        case sub of
            [] -> Nothing
            _ -> Just ("s",toJSON sub)
      ]

instance ToJSON ValueRepr where
    toJSON (VRBit c)    = enumJSON "B" (c:"")
    toJSON (VRBits s)   = enumJSON "V" s
    toJSON (VRString s) = enumJSON "S" s
    toJSON VRNotPresent = show "N"

instance ToJSON ValueKind where
    toJSON VKNormal     = show "N"
    toJSON VKUndef      = show "U"
    toJSON VKHighImp    = show "Z"
    toJSON VKWarn       = show "E"
    toJSON VKDontCare   = show "D"
    toJSON VKWeak       = show "W"
    toJSON (VKCustom c) = enumJSON "C" c

instance ToJSON Color where
    toJSON (RGB r g b) = printf "[%d,%d,%d,255]" r g b

instance ToJSON [SubFieldTranslationResult] where
    toJSON xs = go $ map toJSON xs
        where go [] = "[]"
              go (p:ps) = "[" ++p++ foldr (\a b -> ',':a++b) "]" ps

instance ToJSON [(String,VariableInfo)] where
    toJSON xs = go $ map toJSON xs
        where go [] = "[]"
              go (p:ps) = "[" ++ p ++ foldr (\a b -> ',':a++b) "]" ps


instance ToJSON SubFieldTranslationResult where
    toJSON (SubFieldTranslationResult name res) = structJSON [("n",show name), ("r",toJSON res)]

instance ToJSON VariableInfo where

    toJSON (VICompound xs) = structJSON [("P",structJSON [("s",toJSON xs)])]
    toJSON VIBits   = show "V"
    toJSON VIBool   = show "B"
    toJSON VIClock  = show "C"
    toJSON VIString = show "S"
    toJSON VIReal   = show "R"

instance ToJSON (String,VariableInfo) where
    toJSON (name,info) = "["++show name++","++toJSON info++"]"

instance (ToJSON a) => ToJSON (VariableInfo,a) where
    toJSON (info,x) = "["++toJSON info++","++toJSON x++"]"

instance (ToJSON a) => ToJSON (Map String a) where
    toJSON mp = structJSON $ map (\(k,v) -> (k,toJSON v)) $ toList mp

