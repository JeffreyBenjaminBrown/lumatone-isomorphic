module Colors where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


color :: MidiNote -> Color
color = maybe "000000" id . flip M.lookup color_map

-- PITFALL: These work out nicely for 41-edo.
-- For some other edo, they might not.
color_map :: Map MidiNote Color
color_map = M.fromList [
  -- "naturals"
  ( 0, "aaaaaa"),
  ( 7, "ff0000"),
  (14, "ff0000"),
  (17, "ff0000"),
  (24, "ff0000"),
  (31, "ff0000"),
  (38, "ff0000"),

  ( 1, "aaaaaa"),
  ( 8, "00ff00"),
  (15, "00ff00"),
  (18, "00ff00"),
  (25, "00ff00"),
  (32, "00ff00"),
  (39, "00ff00"),

  ( 2, "aaaaaa"),
  ( 9, "0000ff"),
  (16, "0000ff"),
  (19, "0000ff"),
  (26, "0000ff"),
  (33, "0000ff"),
  (40, "0000ff"),

  ( 3, "aaaaaa"),
  (10, "aaaa00"),
  (17, "aaaa00"),
  (20, "aaaa00"),
  (27, "aaaa00"),
  (34, "aaaa00"),
  (41, "aaaa00") ]
