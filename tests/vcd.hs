
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}


module Main where

import Clash.Prelude
import Clash.Shockwaves
import Clash.Shockwaves.LUT
import Clash.Shockwaves.Trace as T

import Tests.Types

import Data.Text as Text
import Data.Proxy
import Data.Typeable
import qualified Data.List as L

createDomain vSystem{vName="Dom50", vPeriod=hzToPeriod 50e6}


undef :: a
undef = Clash.Prelude.undefined

tests =
  [ test "S"  $ values [S,undef]
  , test "M"  $ values [Ma,Mb,Mc,undef]
  , test "F"  $ values [M True 3,M False 3,undef]
  , test "Op" $ values [True ://: (False ://: False), undef ://: (True ://: False), False ://: (True://:undef),undef]
  , test "St" $ values [St{b=3,a=False},St{a=undef,b=0},undef]
  , test "C"  $ values [Red,Green,Blue,undef]
  , test "Mix"$ values [A,B True,C False 1,D{x=True,y= -1},0 :**: (True ://: False),undef]
  , test "L"  $ values [La True False,Lb False True,undef]
  , test "Maybe" $ values [Nothing, Just True, undef]
  , test "Vec 2" $ values [True :> False :> Nil, undef :> undef :> Nil, undef]
  , test "Vec 0" $ values [Nil @Bool, undef]
  ]


--- string annotations

data AnnotateCycles (c::[Symbol]) = AnnotateCycles Int deriving (BitPack,Generic,Typeable)
                                                       deriving Waveform via (WaveformForLUT (AnnotateCycles c))
getAnnotation :: (SymbolVals c) => AnnotateCycles c -> String
getAnnotation (AnnotateCycles k :: AnnotateCycles c) = (symbolVals (Proxy @c) <> L.repeat "") L.!! k

instance (SymbolVals c, Typeable c) => WaveformLUT (AnnotateCycles c) where
  labelL a = getAnnotation a
  precL _ = 10

annotateC :: (SymbolVals c, KnownDomain dom, HiddenClockResetEnable dom) => Signal dom (AnnotateCycles c)
annotateC = AnnotateCycles <$> counter
  where counter = register 0 (counter + 1)

class SymbolVals a where
  symbolVals :: proxy a -> [String]
instance SymbolVals '[] where
  symbolVals _ = []
instance (KnownSymbol h, SymbolVals t) => SymbolVals (h ': t) where
  symbolVals _ = symbolVal (Proxy :: Proxy h) : symbolVals (Proxy :: Proxy t)

----



type R = Unsigned 8
type R' = Signal Dom50 R

values :: (Waveform a,NFDataX a) => [a] -> String -> R' -> R'
values vals name i = o
  where
    o = i --seq x i
    -- x = T.traceSignal1 (":"<>name) $ fromList $ (L.map showX vals)
    x = T.traceSignal1 name $ fromList $ vals <> L.repeat undef

test :: String -> (String -> R' -> R') -> (String,R'->R')
test name f = (name,f name)


topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  R' ->
  R'
topEntity = exposeClockResetEnable id --runTests

runTests :: R' -> R'
runTests i = L.foldl go i tests'
  where
    tests' = L.map snd tests
    go i' f = f i'



main :: IO ()
main = do
  putStrLn "start"
  let out = topEntity (clockGen @Dom50) (resetGen @Dom50) enableGen $ T.traceSignal1 "helper" $ fromList $ L.repeat (3::Unsigned 8)
  vcddata <- T.dumpVCD (0, 100) out ["helper"]--(L.map fst tests)
  case vcddata of
    Left msg ->
      error msg
      putStrLn "finished with error"
    Right (vcd,meta) ->
      do writeFile "trace/waveform.vcd" $ Text.unpack vcd
         writeFileJSON "trace/waveform.json" meta
         putStrLn "finished"