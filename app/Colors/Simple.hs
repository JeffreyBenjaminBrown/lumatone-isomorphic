module Colors.Simple where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


-- * Simple colors

color_white, color_gray, color_black :: ColorString
color_b, color_g, color_r, color_gb, color_rb, color_rg :: ColorString
color_white = "ffffff"
color_gray  = "888888" -- A dimmer version of white.
color_black = "000000" -- All LEDs off. Looks gray-brown.
color_r   = "ff0000"
color_g   = "00ff00"
color_b   = "0000ff"
color_rg  = "bbbb00" -- yellow
color_rb  = "bb00bb" -- purple
color_gb  = "00bb77" -- TODO: Is this still indistinct? Earlier, as 00bbbb, it close to both blue and white.

colors :: Map Int ColorString
colors = M.fromList [
  (0, color_r  ),
  (1, color_g  ),
  (2, color_b  ),
  (4, color_rg ),
  (3, color_rb ),
  (5, color_gb )
  ]

-- | This, the default color,
-- which will be applied to every EDO value for which
-- `color_map` does not define a color.
default_color :: String
default_color = color_black
