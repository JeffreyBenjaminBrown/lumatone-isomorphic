{-# LANGUAGE ScopedTypeVariables #-}

module Colors.Rainbow where

import           Data.Char
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSL (hsl)
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map   as M
import           Numeric

import Util
import Types


rainbowOfFifths :: Edo
  -> Int -- ^ how many microtones are in a 3:2 interval
  -> Int -- ^ length of the chain of fifths
  -> Map MidiNote ColorString
rainbowOfFifths edo fifth len =
  M.fromList $
  [ (fifth * i `mod` edo, c)
  | i <- [0..len-1],
    let c = rainbowColor $ fromIntegral i / fromIntegral len ]

-- | ASSUMES input is in the open interval [0,1).
-- (1 should give the same output as 0.)
-- Multiply by 2*pi to think of the input as an angle.
rainbowColor :: Float -> ColorString
rainbowColor f = let
  rgb :: RGB Float = hsl (360 * f) 1 0.5
  in concat [ colorUnitFloat_toHex $ f rgb
            | f <- [ channelRed,
                     channelBlue,
                     channelGreen ] ]

-- | ASSUMES input is in [0,1].
-- Output is in ["00","ff"]
colorUnitFloat_toHex :: Float -> String
colorUnitFloat_toHex f =
  let n = round $ 255 * f
  in pad0 2 $ showIntAtBase 16 intToDigit n ""
