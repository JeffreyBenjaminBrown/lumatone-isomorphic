-- import Colors.Edo58 (theMap)

module Colors.Edo58 -- (theMap)
where

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

-- S.difference (S.fromList [0..58]) (S.fromList $ concat chains)
-- let x = [0,8,16,24] in x ++ map (+6) x
separators = [0,8,16,24,6,14,22,30]

-- sharp = mod (7 * 34) 58
-- myPrint $ [ [ mod (i*34 + j*sharp) 58 | i <- [0..6]] | j <- [0..3]]
chains =
  [ [0,34,10,44,20,54,30]
  , [6,40,16,50,26,2,36]
  , [12,46,22,56,32,8,42]
  , [18,52,28,4,38,14,48]
  ]
