module Colors where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


color :: MidiNote -> Color
color = maybe "000000" id . flip M.lookup color_map

-- PITFALL: These are piano-like for 41-edo.
-- For some other edo, they probably aren't.
color_map :: Map MidiNote Color
color_map = M.fromList [
  -- "naturals"
  ( 0, "ffffff"),
  ( 7, "ff0000"),
  (14, "ff0000"),
  (17, "ff0000"),
  (24, "ff0000"),
  (31, "ff0000"),
  (38, "ff0000"),

  -- "flats"
  (37, "ffffff"),
  (3 , "00ff00"),
  (10, "00ff00"),
  (13, "00ff00"),
  (20, "00ff00"),
  (27, "00ff00"),
  (34, "00ff00"),

  -- "sharps"
  (4 , "ffffff"),
  (11, "0000ff"),
  (18, "0000ff"),
  (21, "0000ff"),
  (28, "0000ff"),
  (35, "0000ff"),
  (1 , "0000ff") ]
