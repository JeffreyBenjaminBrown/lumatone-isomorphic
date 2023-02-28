{-# LANGUAGE ScopedTypeVariables #-}

module Colors.Wheel where

import           Data.Char
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSL (hsl)
import qualified Data.List as L
import           Numeric

import Util
import Types


-- | ASSUMES input is in [0,1].
-- Output is in ["00","ff"]
colorUnitFloat_toHex :: Float -> String
colorUnitFloat_toHex f =
  let n = round $ 255 * f
  in pad0 2 $ showIntAtBase 16 intToDigit n ""

-- | ASSUMES input is in the open interval [0,1).
-- (1 should give the same output as 0.)
-- Multiply by 2*pi to think of the input as an angle.
wheelColor :: Float -> ColorString
wheelColor f = let
  rgb :: RGB Float = hsl (360 * f) 1 0.5
  in concat [ colorUnitFloat_toHex $ f rgb
            | f <- [ channelRed,
                     channelBlue,
                     channelGreen ] ]
