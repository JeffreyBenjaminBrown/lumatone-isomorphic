module Colors where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


color :: MidiNote -> Color
color = maybe "000000" id . flip M.lookup color_map

color_map :: Map MidiNote Color
color_map = M.fromList [
  ( 0, "ffffff"),
  ( 7, "ff0000"),
  (14, "ff0000"),
  (17, "ff0000"),
  (24, "ff0000"),
  (31, "ff0000"),
  (38, "ff0000"),

  ( 2, "ffffff"),
  ( 9, "00ff00"),
  (16, "00ff00"),
  (19, "00ff00"),
  (26, "00ff00"),
  (33, "00ff00"),
  (40, "00ff00"),

  (4 , "ffffff"),
  (11, "0000ff"),
  (18, "0000ff"),
  (21, "0000ff"),
  (28, "0000ff"),
  (35, "0000ff"),
  (42, "0000ff") ]
