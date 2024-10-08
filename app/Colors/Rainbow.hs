{-# LANGUAGE ScopedTypeVariables #-}

module Colors.Rainbow where

import           Control.Lens
import           Data.Char
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSL (hsl)
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Set (Set)
import qualified Data.Set   as S
import           Numeric

import Util
import Types


-- | TODO : Generalize this and incorporate it into
-- `units_avoiding_bad_and_finding_good`.
-- Currently it creates the ring of first and second degree neighbors
-- of the root in 53-edo using my 7 right 3 down layout,
-- and then prints a ranking of the color-spacing intervals
-- by how long they take to avoid that double-ring,
-- showing how long they take also to find 3:2 = 31\53 and 11:8 = 37\53.
doubleRingCalculation = let
  immed = [3,4,7]
  immed_and_negs = immed ++ map (*(-1)) immed
  ring2 = immed_and_negs ++ [ a + b |
                                    a <- immed_and_negs,
                                    b <- immed_and_negs]
    in myPrint $ units_avoiding_bad_and_finding_good 53 ring2 [31,37]

-- | TODO:
-- Incorporate the idea in `doubleRingCalculation` above.
--
-- PITFALL:
-- Only considers (edo,unit) pairs that are relatively prime.
-- Other pairs may, however, also be of interest.
--
-- PURPOSE:
-- Run `reps_until_entering` a lot,
-- to find nice values of `unit`.
-- See type signature for details.
--
-- EXAMPLE:
-- Sort units by how long they take to avoid
-- immediate or 2nd-degree neighbors of the root
-- in my favorite 53-edo layout (7\53 right by -3\53 down),
-- and list how long they take to find the perfect fifth (31\53)
-- and 13th harmonic (37\53):
--   myPrint $ units_avoiding_bad_and_finding_good 53 [0,1,3,4,6,7,8,10,11,14] [31,37]
-- Similar, for 34-edo:
--   myPrint $ units_avoiding_bad_and_finding_good 34 [3,8,11,6,16,14,22,19,29] [20]
units_avoiding_bad_and_finding_good ::
  Edo ->
  [Int] -> -- ^ The "bad trap". Values to avoid for as long as possible. The list is sorted on the length it takes to avoid these.
  [Int] -> -- ^ The "good trap". Values I would like to be easy to see, hence to arrive at as quickly as possible.
  [ (Int, Int, [Int]) ] -- ^ (unit, bad_time, good_times) triples. `bad_time` indicates how long it takes to reach the bad trap (ideally, should be big). `good_times` indicates how long it takes to reach the good traps (ideally, should be small).
units_avoiding_bad_and_finding_good edo bad good =
  let f unit = ( unit,
                 reps_until_entering edo bad unit,
                 [ reps_until_entering edo [g] unit |
                   g <- good ] )
  in L.sortOn (^. _2)
     [ f unit
     | unit <- [1..ceiling (fromIntegral edo / 2)],
       relativelyPrime unit edo ]

-- | FUNCTION:
-- `reps_until_entering edo unit trap`
-- determines how many times `unit` must be summed (starting from 0)
-- in `edo` until its absolute value is in `trap`.
--
-- PURPOSES (there are 2):
-- On the Lumatone, it can be desirable to space out adjacent
-- by an interval (`unit`) which causes the colors to
-- take a long time to come within one key of where they started.
-- By using a `trap` value equal to a representation of
-- the values in that Edo of the six notes adjancent to the root,
-- one can find a good `unit` interval.
-- (That representation need not include the negative values.)
--
-- Once `unit` values that look good by that criterion are found,
-- this function can be used to narrow the field further,
-- by determining, e.g., how long such a given `unit` takes to land
-- on intervals I would like to be able to identify easily.
--
-- PITFALL: The values that belong in `trap` depend not just on the edo,
-- but on the layout being used.
--
-- EXAMPLES:
-- Purpose 1:
-- To find how long a unit of 31 \ 53 (the perfect fifth)
-- takes to come within 1 key of the root
-- using the (my favorite) 7-right 3-up layout for 53-edo:
--   reps_until_entering 53 [7,3,4] 31
--
-- Purpose 2:
-- Find how long it takes for a unit of 21 \ 53
-- to land on 31, which is the perfect fifth in 53-edo:
--   reps_until_entering 53 [31] 21
reps_until_entering ::
  Edo ->
  [Int] -> -- ^ Values to trap -- intended to represent notes adjacent to the 0. PITFALL: Only needs their absolute values, so 3 values, not 6.
  Int -> -- ^ The unit to test.
  Int
reps_until_entering edo trap unit =
  let trap_and_octave_inverses =
        trap ++ [ edo - a | a <- trap]
  in fst . head
     $ dropWhile (\(_,rep) -> not $ rep `elem` trap_and_octave_inverses)
     $ zip [1..]
     [ mod (i * unit) edo
     | i <- [1 ..edo] ]

{-^ Easiest understood through example
    plus reading the type signature and its comments).
rainbow 12 4 3 0 = Map.fromList
                  [(0,"ff0000"),(4,"0000ff"),(8,"00ff00")]
rainbow 12 4 3 1 = Map.fromList
                  [(1,"ff0000"),(5,"0000ff"),(9,"00ff00")]
-}
rainbow :: Edo
  -> Int -- ^ The number of microtones by which to space out adjacent colors. To find a good value of this, use `units_avoiding_bad_and_finding_good`.
  -> Int -- ^ length of the chain of units.
  -> Int -- ^ offset
  -> Map MidiNote ColorString
rainbow edo unitInterval len offset =
  M.fromList $
  [ (mod (unitInterval * i + offset) edo, c)
  | i <- [0..len-1],
    let c = rainbowColor $ fromIntegral i / fromIntegral len ]

-- | ASSUMES input is in the open interval [0,1).
-- (1 should give the same output as 0.)
-- Multiply by 2*pi to think of the input as an angle.
rainbowColor :: Float -> ColorString
rainbowColor f = let
  rgb :: RGB Float = hsl (360 * f) 1 0.5
  in concat [ colorUnitFloat_toHex $ f rgb
            | f <- [ channelRed,
                     channelBlue,
                     channelGreen ] ]

-- | ASSUMES input is in [0,1].
-- Output is in ["00","ff"]
colorUnitFloat_toHex :: Float -> String
colorUnitFloat_toHex f =
  let n = round $ 255 * f
  in pad0 2 $ showIntAtBase 16 intToDigit n ""
