module Colors.Edo46 where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


-- | PITFALL: This is not good for every 46-edo layout,
-- but it seems good for 4\46-right x 11\46-downright.
-- Going down by one vertical key in that layout
-- corresponds to going up in pitch 7\46 steps,
-- so this layout stacks 5 isomorphic chains of fifths vertically.
theMap :: [(MidiNote, Color)]
theMap = [
  (0  , white_max),
  (27 , color_r  ),
  (8  , color_r  ),
  (35 , color_r  ),
  (16 , color_r  ),
  (43 , color_r  ),
  (24 , color_r  ),

  (7  , white_max),
  (34 , color_g  ),
  (15 , color_g  ),
  (42 , color_g  ),
  (23 , color_g  ),
  (4  , color_g  ),
  (31 , color_g  ),

  (14 , white_max),
  (41 , color_b  ),
  (22 , color_b  ),
  (3  , color_b  ),
  (30 , color_b  ),
  (11 , color_b  ),
  (38 , color_b  ),

  (21 , white_max),
  (2  , color_rg ),
  (29 , color_rg ),
  (10 , color_rg ),
  (37 , color_rg ),
  (18 , color_rg ),
  (45 , color_rg ),

  (28 , white_max),
  (9  , color_gb ),
  (36 , color_gb ),
  (17 , color_gb ),
  (44 , color_gb ),
  (25 , color_gb ),
  (6  , color_gb ) ]
