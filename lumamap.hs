import Data.Map (Map)
import Data.Map as M


type Board = Int -- ^ The Lumatone has five, numbered 0 through 4.

type Key = Int -- ^ Each Board has 56, numbered 0 to 55.
-- Refer to this picture on p. 28 of the Lumatone manual for which is where:
-- [[~/many-small/music-making/lumatone/2022-08-27-stl4uf7J.one-lumatone-octave.png]]

-- | These are computer graphics-style coordinates:
-- x runs left to right, and y runs *up* to *down*.
type X = Int
type Y = Int

key_positions :: Map Int (Int, Int)
key_positions = M.fromList [
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

  (37, (-3, 8)),
  (38, (-2, 8)),
  (39, (-1, 8)),
  (40, ( 0, 8)),
  (41, ( 1, 8)),
  (42, ( 2, 8)),

  (43, (-4, 9)),
  (44, (-3, 9)),
  (45, (-2, 9)),
  (46, ( 1, 9)),
  (47, ( 0, 9)),
  (48, ( 1, 9)),

  (49, (-3,10)),
  (50, (-2,10)),
  (51, (-1,10)),
  (52, ( 0,10)),
  (53, ( 1,10)),

  (54, (-1,11)),
  (55, ( 0,11)) ]

all_280_keys :: [ (Board, Key) ]
all_280_keys = [ (b,k)
               | b <- [0..4],
                 k <- [0..55] ]
