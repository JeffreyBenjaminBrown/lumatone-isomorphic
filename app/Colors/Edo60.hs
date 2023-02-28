module Colors.Edo60 where

import           Data.Map (Map)
import qualified Data.Map   as M

import Colors.Simple
import Types


theMap :: Map MidiNote ColorString
theMap = let
  f :: ColorString -> [MidiNote] -> [(MidiNote,ColorString)]
  f c = map $ \n -> (n,c)
  in M.fromList $
     (concat $ zipWith f (M.elems colors) theChainsOfFifths)
     ++ zip [5,7..19] (repeat color_white)

theChainsOfFifths =
  [ [ mod (s * 2 + -- 2\edo between chains stacks them vertically
                   -- when I use 7\60 x 2\60
            positionInChain * 35) 60 -- 60-edo's best 3/2 is 35\60
    | positionInChain <- [0..6] ] -- each chain is 7 notes
  | s <- [0..4] ] -- 5 chains
