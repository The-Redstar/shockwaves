
-- TODO: switch to actual Clash, not haskell with a fake bitvector

--{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoNamedFieldPuns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module WaveFormTranslation (genTable, StructF,TransF,TranslationTable,TypeFunctions(..),    BitPack(..),BitVector(..)) where

import WaveFormViewer
import WaveFormJSON

import Data.Map (Map,toList)
import qualified Data.Map as Map

import GHC.Natural
import GHC.TypeLits (KnownNat)

-- TEMPORARY for testing purposes
import GHC.Generics

-- the function signatures, so we can easily return the functions for the specified type
type StructF = VariableInfo
type TransF = (String -> TranslationResult)

type TranslationTable = Map String (VariableInfo,Map String TranslationResult)

-- convert bit string to BitVector
toBV :: (KnownNat n) => String -> BitVector n
toBV s = BV m i
    where
        toNat :: String -> (Natural,Natural)
        toNat "" = (0,0)
        toNat (c:rest) = (da+2*a,db+2*b)
            where
                (a,b) = toNat rest
                (da,db) = case c of
                    '1' -> (0,1)
                    '0' -> (0,0)
                    _   -> (1,0)
        (m,i) = toNat $ reverse s

-- get functions of a type
class TypeFunctions a where
    tf :: (StructF,TransF)
instance (BitPack a,Split a) => TypeFunctions a where
    tf = (structure @a,translate')
        where translate' val = translate @a $ unpack (toBV val::(BitVector (BitSize a)))

-- generate a table of value representations for types
-- input [(typename,[values])]
genTable :: (String -> (StructF,TransF)) -> [(String,[String])] -> TranslationTable
genTable typeFunc typeValues = Map.fromList $ map genTable' typeValues
    where
        genTable' :: (String,[String]) -> (String,(VariableInfo, Map String TranslationResult))
        genTable' (ty,vals) = (ty, (struct, Map.fromList $ zip vals $ map trans vals))
            where (struct,trans) = typeFunc ty



------------------- TEST STUFF (so I can use pure haskell in testing)





class BitPack a where
    unpack :: BitVector (BitSize a) -> a
type BitSize a = 5
data BitVector a = BV Natural Natural
