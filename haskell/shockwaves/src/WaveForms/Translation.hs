
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoNamedFieldPuns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module WaveForms.Translation (
    genTable, StructF,TransF,TranslationTable, -- generating translations
    TypeFunctions(tf),                         -- matching types
    translateFile,translateCmdLine             -- file input output
) where

import Prelude
import System.Environment (getArgs)

import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Natural
import GHC.TypeLits (KnownNat)

import Clash.Sized.Internal.BitVector (BitVector(BV))
import Clash.Class.BitPack   (BitPack,BitSize,unpack)

import WaveForms.Viewer
import WaveForms.JSON

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


translateFile :: (String -> (StructF, TransF)) -> String -> String -> IO ()
translateFile types infile outfile = do
    putStrLn $ "Reading file: "++infile
    content <- readFile infile
    
    let translationTable = genTable types $ map splitSnd $ pairs $ lines content
        pairs (x:y:r) = (x,y):pairs r
        pairs _       = []
        splitSnd (a,b) = (a,words b)

    let json = toJSON translationTable

    putStrLn "Result"
    putStrLn $ take 200 json ++ "..."
    putStrLn $ "Saving to file: "++outfile
    writeFile outfile json


translateCmdLine :: (String -> (StructF, TransF)) -> IO ()
translateCmdLine types = do
    args <- getArgs
    let infile = args !! 0
    let outfile = args !! 1
    translateFile types infile outfile

