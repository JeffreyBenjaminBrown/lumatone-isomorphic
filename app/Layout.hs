module Layout where

import           Data.Map (Map)
import qualified Data.Map   as M

import Types


-- | The idea in `key_xy_map` is to provide (x,y) coordinates for each key of the Lumatone. I'm calling the top-left corner is (0,0), because that is for some reason standard in computer graphics. Moving right increases x by 1, and moving down-left increases y by 1. Therefore there are a few keys with y-coordinates "behind" the origin -- that is, to the left of it in this coordinate system, albeit not in reality.
key_xy_map :: Map Key (X,Y)
key_xy_map = M.fromList [
  ( 0, ( 0, 0)),
  ( 1, ( 1, 0)),

  ( 2, ( 0, 1)),
  ( 3, ( 1, 1)),
  ( 4, ( 2, 1)),
  ( 5, ( 3, 1)),
  ( 6, ( 4, 1)),

  ( 7, (-1, 2)),
  ( 8, ( 0, 2)),
  ( 9, ( 1, 2)),
  (10, ( 2, 2)),
  (11, ( 3, 2)),
  (12, ( 4, 2)),

  (13, (-1, 3)),
  (14, ( 0, 3)),
  (15, ( 1, 3)),
  (16, ( 2, 3)),
  (17, ( 3, 3)),
  (18, ( 4, 3)),

  (19, (-2, 4)),
  (20, (-1, 4)),
  (21, ( 0, 4)),
  (22, ( 1, 4)),
  (23, ( 2, 4)),
  (24, ( 3, 4)),

  (25, (-2, 5)),
  (26, (-1, 5)),
  (27, ( 0, 5)),
  (28, ( 1, 5)),
  (29, ( 2, 5)),
  (30, ( 3, 5)),

  (31, (-3, 6)),
  (32, (-2, 6)),
  (33, (-1, 6)),
  (34, ( 0, 6)),
  (35, ( 1, 6)),
  (36, ( 2, 6)),

  (37, (-3, 7)),
  (38, (-2, 7)),
  (39, (-1, 7)),
  (40, ( 0, 7)),
  (41, ( 1, 7)),
  (42, ( 2, 7)),

  (43, (-4, 8)),
  (44, (-3, 8)),
  (45, (-2, 8)),
  (46, (-1, 8)),
  (47, ( 0, 8)),
  (48, ( 1, 8)),

  (49, (-3, 9)),
  (50, (-2, 9)),
  (51, (-1, 9)),
  (52, ( 0, 9)),
  (53, ( 1, 9)),

  (54, (-1,10)),
  (55, ( 0,10)) ]
