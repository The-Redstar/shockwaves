
-- temporary test script meant to illustrate

import WaveFormViewer
import WaveFormColor
import WaveFormTranslation
import WaveFormJSON
import GHC.Generics

data Test = A | B Int | C{val::Int,var::Int} deriving (Show,Generic)
instance Display Test
instance Split Test

data Test3 = Red | Blue | Green deriving (Show,Generic)
instance Display Test3 where
    kind Red   = VKCustom red
    kind Green = VKCustom green
    kind Blue  = VKCustom blue
instance Split Test3


instance BitPack Test where
    unpack (BV _ 0) = A
    unpack (BV _ 0b0111) = B 3
    unpack (BV _ 0b1010) = C{val=0,var=2}

instance BitPack Test3 where
    unpack (BV _ 0) = Red
    unpack (BV _ 1) = Blue



-- lookup functions of type using its label
types :: String -> (StructF,TransF)
types tag = case tag of
    "Color" -> tf @Test3
    "Test" -> tf @Test

-- translationTable :: TranslationTable
-- translationTable = genTable types [
--         ("Color",["0","1"]),
--         ("Test" ,["0000","0111","1010"])
--     ]

translateFile infile outfile = do
    content <- lines $ readFile infile
    
    let translationTable = map (\(a,b) -> (a, words b)) $ pairs content
        pairs (x:y:r) = (x,y):pairs r
        pairs _       = []

    let json = toJSON translationTable

    putStrLn "Result"
    putStrLn $ take 200 json ++ "..."
    putStrLn $ "Saving to file: "++outfile
    writeFile outfile json


main = do
    args <- getArg
    let infile = args !! 0
    let outfile = args !! 1
    translateFile infile outfile