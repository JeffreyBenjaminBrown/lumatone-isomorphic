module Colors.Simple where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


-- * Simple colors

white_max, white_mid, black :: Color
color_b, color_g, color_r, color_gb, color_rb, color_rg :: Color
white_max = "ffffff"
white_mid = "888888"
black     = "000000"
color_b   = "0000ff"
color_g   = "00ff00"
color_r   = "ff0000"
color_gb  = "00bbbb"
color_rb  = "bb00bb"
color_rg  = "bbbb00"

colors :: Map Int Color
colors = M.fromList [
  (0, color_b  ),
  (1, color_b  ),
  (2, color_g  ),
  (3, color_r  ),
  (4, color_gb ),
  (5, color_rb ),
  (6, color_rg ) ]

-- | This, the default color,
-- which will be applied to every EDO value for which
-- `color_map` does not define a color.
default_color :: String
default_color = black
