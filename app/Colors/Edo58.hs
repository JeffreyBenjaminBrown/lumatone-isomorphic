-- import Colors.Edo58 (theMap)

module Colors.Edo58 (theMap) where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


theMap :: [(MidiNote, Color)]
theMap = let
  f :: Color -> [MidiNote] -> [(MidiNote,Color)]
  f c = map $ \n -> (n,c)
  in concat
     $ zip separators (repeat color_white)
     : zipWith f (M.elems colors) chains

separators = [17,21,24,27]

chains =
  [ [0,34,10,44,20,54,30]
  , [3,37,13,47,23,57,33]
  , [6,40,16,50,26,2,36]
  , [9,43,19,53,29,5,39]
  , [12,46,22,56,32,8,42]
  , [15,49,25,1,35,11,45]
  , [18,52,28,4,38,14,48]
  {- , [21,55,31,7,41,17,51] ]
PITFALL: There's room for one more chain (the above line),
but the result is actually harder to view,
because the last color is near the other colors. -}
  ]
