

module Shockwaves.Internal.Render where

import Clash.Prelude
import Shockwaves.Internal.Types
import qualified Data.List as L

applyStyle :: WaveStyle -> Translation -> Translation
applyStyle s (Translation r sb) = Translation (applyStyleR s r) sb

applyStyleR :: WaveStyle -> Render -> Render
applyStyleR s (Just (l,WSNormal,p)) = Just (l,s,p)
applyStyleR _ r = r

renError :: Value -> Render
renError v = Just ("{" <> v <> "}", WSError, 11)

applyPrec :: Prec -> Translation -> Translation
applyPrec p (Translation r s) = Translation (applyPrecR p r) s

applyPrecR :: Prec -> Render -> Render
applyPrecR p (Just (v,s,p')) = if p'>p then Just (v,s,p')
                               else Just (parenthesize v, s, 11)
applyPrecR _ Nothing = Nothing 

applyPrecL :: Prec -> [(a, Translation)] -> [(a, Translation)]
applyPrecL p = L.map (\(a,b) -> (a,applyPrec p b))

parenthesize :: Value -> Value
parenthesize n = "("<>n<>")"

joinWith :: Value -> [Value] -> Value
joinWith s (x:xs) = x <> joinWith s xs
joinWith _ [] = ""

getVal :: Translation -> Value
getVal t = case t of
              Translation (Just (v,_,_)) _ -> v
              _ -> "{Value missing}"

render :: Translator -> [(SubSignal,Translation)] -> Maybe (Value,WaveStyle,Prec)
render (Translator _ translator) subs = case translator of
  TRef _ -> errorX "Ref should only appear as a nested type that is translated through split; for referenced types, modify Waveform.translate"
  TLut _ -> errorX "LUT translators require a custom implementation of Waveform.translate that does not call render"
  TNumber{} -> errorX "Number translators require a custom implementation of Waveform.translate that does not call render"
  TSum _ -> case subs of
    (_,Translation ren _):_ -> ren
    _ -> renError "failed to split"
  TProduct
    { start, sep, stop
    , labels
    , preci, preco
    } -> Just (v,WSNormal,preco)
      where
        labels' = labels <> L.repeat Nothing
        subs' = applyPrecL preci subs
        vals = L.map (getVal . snd) subs'
        v = start <> joinWith sep fields <> stop
        fields = L.zipWith addLabel labels' vals
        addLabel = \case
          Just l -> (l<>)
          Nothing -> id
  TConst (Translation ren _) -> ren
  TArray
    { elems
    , start, sep, stop
    , preci, preco
    } -> if L.length subs == elems then
           Just (start <> joinWith sep (L.map (getVal . applyPrec preci . snd) subs) <> stop, WSNormal, preco)
         else
           renError "Values missing"

  TStyled sty t -> applyStyleR sty $ render t subs
  TMaybe _ -> case subs of
    (_,Translation ren _):_ -> ren
    _ -> Nothing