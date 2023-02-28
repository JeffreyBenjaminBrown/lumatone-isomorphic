module Colors.Wheel where

import           Data.Char
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSL (hsl)
import qualified Data.List as L
import           Numeric

import Util


-- | ASSUMES input is in [0,1].
-- Output is in ["00","ff"]
colorUnitFloat_toHex :: Float -> String
colorUnitFloat_toHex f =
  let n = round $ 255 * f
  in pad0 2 $ showIntAtBase 16 intToDigit n ""

