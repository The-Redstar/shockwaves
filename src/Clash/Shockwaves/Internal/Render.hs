{-|

Code for rendering values using the translators specified.
Values are constructed from their subvalues.

-}

module Clash.Shockwaves.Internal.Render where

import Clash.Prelude
import Clash.Shockwaves.Internal.Types
import qualified Data.List as L

-- | Apply a 'WaveStyle' to a 'Translation'
applyStyle :: WaveStyle -> Translation -> Translation
applyStyle s (Translation r sb) = Translation (applyStyleR s r) sb

-- | Apply a 'WaveStyle' to a 'Render' value
applyStyleR :: WaveStyle -> Render -> Render
applyStyleR s (Just (l,WSNormal,p)) = Just (l,s,p)
applyStyleR _ r = r

-- | Render some error message. The precedence is set to 11 (i.e. an atomic).
renError :: Value -> Render
renError v = Just (v, WSError, 11)

-- | Apply a precedence value to a 'Translation'.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrec :: Prec -> Translation -> Translation
applyPrec p (Translation r s) = Translation (applyPrecR p r) s

-- | Apply a precedence value to a 'Render'.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrecR :: Prec -> Render -> Render
applyPrecR p (Just (v,s,p')) = if p'>p then Just (v,s,p')
                               else Just (parenthesize v, s, 11)
applyPrecR _ Nothing = Nothing 

-- | Apply a precedence value to a list of subsignal translations.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrecL :: Prec -> [(a, Translation)] -> [(a, Translation)]
applyPrecL p = L.map (\(a,b) -> (a,applyPrec p b))

-- | Wrap parentheses around a value.
parenthesize :: Value -> Value
parenthesize n = "("<>n<>")"

-- | Join a list of values with a separator. If the list is empty, an empty value is returned.
joinWith :: Value -> [Value] -> Value
joinWith s (x:y:r) = x <> s <> joinWith s (y:r)
joinWith _ [x] = x 
joinWith _ [] = ""

-- | Get the value of a 'Translation'. If the value is not defined, return @{value missing}@.
getVal :: Translation -> Value
getVal t = case t of
              Translation (Just (v,_,_)) _ -> v
              _ -> "{Value missing}"

-- | Render a value based on its translated subsignals and assigned translator.
render :: Translator -> [(SubSignal,Translation)] -> Maybe (Value,WaveStyle,Prec)
render (Translator _ translator) subs = case translator of
  TRef _ -> errorX "Ref should only appear as a nested type that is translated through split; for referenced types, modify Waveform.translate"
  TLut _ -> errorX "LUT translators require a custom implementation of Waveform.translate that does not call render"
  TNumber{} -> errorX "Number translators require a custom implementation of Waveform.translate that does not call render"
  TSum _ -> case subs of
    (_,Translation ren _):_ -> ren
    _ -> renError "{invalid variant}"
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
    { len
    , start, sep, stop
    , preci, preco
    } -> if L.length subs == len then
           Just (start <> joinWith sep (L.map (getVal . applyPrec preci . snd) subs) <> stop, WSNormal, preco)
         else
           renError "{values missing}"

  TStyled sty t -> applyStyleR sty $ render t subs
  TMaybe _ -> case subs of
    (_,Translation ren _):_ -> ren
    _ -> Nothing