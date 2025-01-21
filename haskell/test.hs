
-- temporary test script meant to illustrate

import WaveForms.Viewer
import WaveForms.Color
import WaveForms.Translation
import GHC.Generics

data Test = A | B Int | C{val::Int,var::Int} deriving (Show,Generic,BitPack)
instance Display Test
instance Split Test

data Test3 = Red | Blue | Green deriving (Show,Generic,BitPack)
instance Display Test3 where
    kind Red   = VKCustom red
    kind Green = VKCustom green
    kind Blue  = VKCustom blue
instance Split Test3


-- lookup functions of type using its label
types :: String -> (StructF,TransF)
types tag = case tag of
    "Color" -> tf @Test3
    "Test" -> tf @Test


main = translateCmdLine types