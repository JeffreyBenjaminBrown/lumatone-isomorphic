module Colors.Edo31 (theMap)
where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


theMap :: Map MidiNote ColorString
theMap = let
  f :: ColorString -> [MidiNote] -> [(MidiNote,ColorString)]
  f c = map $ \n -> (n,c)
  in M.fromList $ concat
     $ zip separators (repeat color_white)
     : zipWith f (M.elems colors) chains

-- S.difference (S.fromList [0..30]) (S.fromList $ concat chains)
separators = [8,13]

chains =
  -- myPrint $ [ [ mod (i*18 + j*2) 31 | i <- [0..6]] | j <- [0..3]]
  [ [0,18,5,23,10,28,15]
  , [2,20,7,25,12,30,17]
  , [4,22,9,27,14,1,19]
  , [6,24,11,29,16,3,21] ]
