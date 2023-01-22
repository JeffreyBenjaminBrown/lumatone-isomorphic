module Colors.Edo46 where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


-- | This is not great for every 46-edo layout
-- (and probably not even most of them)
-- but it's *great* for 4\46-right x 11\46-downright.
-- Going down by one vertical key in that layout
-- corresponds to going up in pitch 7\46 steps,
-- so this layout stacks 5 chains of fifths vertically.
theMap :: [(MidiNote, Color)]
theMap = [
  (0  , color_rb ),
  (27 , color_rb ),
  (8  , color_rb ),
  (35 , color_rb ),
  (16 , color_rb ),
  (43 , color_rb ),
  (24 , color_rb ),

  (7  , color_g  ),
  (34 , color_g  ),
  (15 , color_g  ),
  (42 , color_g  ),
  (23 , color_g  ),
  (4  , color_g  ),
  (31 , color_g  ),

  (14 , color_b  ),
  (41 , color_b  ),
  (22 , color_b  ),
  (3  , color_b  ),
  (30 , color_b  ),
  (11 , color_b  ),
  (38 , color_b  ),

  (21 , color_r  ),
  (2  , color_r  ),
  (29 , color_r  ),
  (10 , color_r  ),
  (37 , color_r  ),
  (18 , color_r  ),
  (45 , color_r  ),

  (28 , color_rg ),
  (9  , color_rg ),
  (36 , color_rg ),
  (17 , color_rg ),
  (44 , color_rg ),
  (25 , color_rg ),
  (6  , color_rg ),

  -- These serve to visually separate the chains of fifths.
  -- They are not themselves a chain of fifths.
  (1,  color_white),
  (40, color_white),
  (33, color_white),
  (26, color_white),
  (19, color_white),
  (12, color_white),
  (5,  color_white) ]
