import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Shockwaves
import Shockwaves.Viewer
import Shockwaves.JSON
import Clash.Prelude hiding (split)

main :: IO ()
main = defaultMain $ tests



data P = Q Int deriving (Show,Display,Generic,Split)
data T = A | B Int deriving (Show,Generic,Split)
instance Display T where
  kind A = VKWarn
  kind _ = VKNormal

tests = testGroup "Tests" [displayT,splitT,structureT,jsonT]

undef = Clash.Prelude.undefined

displayT = testGroup "Display"
  [ testCase "Single constructor" $ display (Q 5)         @?= (VRString "Q 5",VKNormal)
  , testCase "Double constructor" $ display  A            @?= (VRString "A"  ,VKWarn)
  , testCase "Undefined child"    $ safeDisplay (B undef) @?= Nothing
  ]

splitT = testGroup "Split"
  [ testCase "Single constructor" $ split (Q 5)                  (VRString "?",VKWeak) @?= [SubFieldTranslationResult "0" (TranslationResult (VRString "5",VKNormal) [])]
  , testCase "Double constructor" $ split  A                     (VRString "?",VKWeak) @?= [SubFieldTranslationResult "A" (TranslationResult (VRString "?",VKWeak  ) [])]
  , testCase "Undefined child"    $ split (B undef)              (VRString "?",VKWeak) @?= [SubFieldTranslationResult "B" (TranslationResult (VRString "?",VKWeak) [SubFieldTranslationResult "0" (TranslationResult (VRString "undefined",VKUndef) [])])]
  , testCase "Vector"             $ split (True :> False :> Nil) (VRString "?",VKWeak) @?= [SubFieldTranslationResult "0" (TranslationResult (VRBit '1',VKNormal) []),SubFieldTranslationResult "1" (TranslationResult (VRBit '0',VKNormal) [])]
  ]

structureT = testGroup "Structure"
  [ testCase "Single constructor" $ structure @P @?= VICompound [("0",VIString)]
  , testCase "Double constructor" $ structure @T @?= VICompound [("A",VIString),("B",VICompound [("0",VIString)])]
  ]

jsonT = testGroup "JSON conversion"
  [ testCase "Single constructor" $ (toJSON $ translate (Q 5)) @?= quotes "{'v':{'S':'Q 5'},'s':[{'n':'0','r':{'v':{'S':'5'}}}]}"
  , testCase "Double constructor" $ (toJSON $ translate  A   ) @?= quotes "{'v':{'S':'A'},'k':'E','s':[{'n':'A','r':{'v':{'S':'A'},'k':'E'}}]}"
  ]

quotes :: String -> String
quotes ('\'':rest) = '"':quotes rest
quotes (c:rest) = c:quotes rest
quotes "" = ""