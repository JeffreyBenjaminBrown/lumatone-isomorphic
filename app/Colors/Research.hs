module Colors.Research where

import           Data.List (sortOn)
import           Data.Set (Set)
import qualified Data.Set   as S

import Types


chainOfFifths_spacingMenu'
  :: Edo
  -> Int -- ^ How many steps of the edo best approximate 3:2.
  -> Int -- ^ number of chains to try to fit
  -> Int -- ^ how much space to put between chains
  -> Int -- ^ How many distinct notes result. If it's small, the chains are clobbering each other.
chainOfFifths_spacingMenu'  edo fifth nChains shiftSize =
  length $ S.toList $ S.fromList $ concat
  [ [ mod (s * shiftSize + positionInChain * fifth) edo
    | positionInChain <- [0..6] ]
  | s <- [0..nChains-1] ]

{- | How much space (in the edo) to put
between equally-spaced chains of fifths
to maximize the number that fit on the Lumatone without overlapping.

TODO : Replace this with the poorly-documented
@chainOfFifths_spacingMenu'@ above.
-}
chainOfFifths_spacingMenu
  :: Edo
  -> Int -- ^ How big a fifth is. TODO: Automate.
  -> [(Int, -- ^ How much space is between each chain of fifths.
       Int)] -- ^ How many notes the hcains will cover.
chainOfFifths_spacingMenu edo fifth =
  let x z = S.toList $ S.fromList $
            concat [ [ mod (i*fifth + j*z) edo
                     | i <- [0..6] ]
                   | j <- [0..7] ]
  in sortOn snd
     [ (z, length $ x z)
     | z <- [1.. edo `div` 2] ]
