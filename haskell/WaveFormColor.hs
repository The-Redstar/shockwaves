
module WaveFormColor (
    Color(..),
    red,yellow,green,cyan,blue,magenta,white,gray,
) where


data Color = RGB Int Int Int deriving Show

red     = RGB 255 0 0
yellow  = RGB 255 255 0
green   = RGB 0 255 0
cyan    = RGB 0 255 255
blue    = RGB 0 0 255
magenta = RGB 255 0 255
white   = RGB 255 255 255
gray    = RGB 127 127 127