module Colors.Edo29 (theMap)
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
separators = [28,1,3]

chains =
  -- myPrint $ [ [ mod (i*17 + j*2) 29 | i <- [0..6]] | j <- [0..2]]
  [ [0,17,5,22,10,27,15]
  , [2,19,7,24,12,0,17]
  , [4,21,9,26,14,2,19] ]
