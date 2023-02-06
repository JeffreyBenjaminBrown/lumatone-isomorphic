module Colors.Edo58 where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


theMap :: [(MidiNote, Color)]
theMap = let
  f :: Color -> [MidiNote] -> [(MidiNote,Color)]
  f c = map $ \n -> (n,c)
  in concat $ zipWith f (M.elems colors)
     [ [0,34,10,44,20,54,30]
     , [3,37,13,47,23,57,33]
     , [6,40,16,50,26,2,36]
     , [9,43,19,53,29,5,39]
     , [12,46,22,56,32,8,42]
     , [15,49,25,1,35,11,45]
     , [18,52,28,4,38,14,48]
     , [21,55,31,7,41,17,51] ]
