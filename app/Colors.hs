{-# LANGUAGE ScopedTypeVariables #-}

module Colors where

import           Data.List.Extra (maximumOn)
import           Data.List (sortOn)
import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Set (Set)
import qualified Data.Set   as S

import Colors.Simple
import Types

{- | PITFALL:
I defined color maps for the following edos "by hand" (-ish).

That's because, at least for non-Bosanquet layouts
(and maybe even some Bosanquet ones --
I only remember having tested with 41-edo and 53-edo)
the function `chains_of_fifths` turns out not to be very useful.
It defines chains of fifths well enough,
but nothing guarantees that the white notes that start each chain
form a pattern that repeats recognizably every octave,
the way it does for the Bosanquet layouts I've tested.

This is probably the way to go with other edos, too,
if their default colors are confusing.
-}
import Colors.Edo29
import Colors.Edo31
import Colors.Edo34
import Colors.Edo46
import Colors.Edo58
import Colors.Edo60


-- | Identifies the note corresponding to a key near the top
-- of the middle board, and about half a board to the left of
-- the note identified by @middlish_high_key_note@.
middlish_low_key_note :: Map (Board, Key) EdoNote -> EdoNote
middlish_low_key_note m = let
  (b,k) = (2,22) {- Key 22 is in the middle of the top.  Board 2 is right in the middle. Using the middle board means that even if the layout is skew, it'll be near the middle of the top on all five boards. -}
  errMsg = "middlish_low_key_note: (board,key) "
           ++ show (b,k) ++ "not found."
  in maybe (error errMsg) id $ M.lookup (b,k) m

-- | Identifies the note corresponding to a key near the bottom
-- of the middle board, and about half a board to the right of
-- the note identified by @middlish_low_key_note@.
middlish_high_key_note :: Map (Board, Key) EdoNote -> EdoNote
middlish_high_key_note m = let
  (b,k) = (2,22) {- Key 22 is in the middle of the bottom.  Board 2 is right in the middle. Using the middle board means that even if the layout is skew, it'll be near the middle of the bottom on all five boards. -}
  errMsg = "middlish_high_key_note: (board,key) "
           ++ show (b,k) ++ "not found."
  in maybe (error errMsg) id $ M.lookup (b,k) m

{- | Modifies a @Map MidiNote ColorString@ by adding black and white notes.
The two black notes will, at least if octaves are close to Bosanquet,
be around the middle of the bottom-right of each board,
and the white notes the middle of the top-left.
-}
overlay_blackAndWhite ::
  EdoNote
  -> Map (Board, Key) EdoNote
  -> Map MidiNote ColorString
  -> Map MidiNote ColorString
overlay_blackAndWhite up boards = let
  low  :: EdoNote = middlish_low_key_note boards
  high :: EdoNote = middlish_high_key_note boards
  overlay = M.fromList [ (low, color_black),
                         (low + up, color_black),
                         (high, color_black),
                         (high + up, color_white) ]
  in M.union overlay


-- * Mapping notes to colors

-- | The Edo determines the color map used.
--
-- PITFALL: If it's not an Edo I've made a color map for,
-- I just give it the 50-edo color map,
-- since that's got the maximum possible number of note-color
-- pairs (49, given that there are only 7 colors and 7 notes
-- per color) and is empty almost nowhere.
color :: Edo -> MidiNote -> ColorString
color edo note =
  let color_map :: Map MidiNote ColorString
      color_map = maybe colors_for_50_edo id $
                  M.lookup edo color_maps
  in maybe default_color id $
     M.lookup note color_map

-- | This assigns colors to EDO values.
-- Not every EDO value needs to have a color assigned,
-- and they do not need to be defined in order.
--
-- These colors work out nicely for Bosanquet 41-edo.
-- For some other edo and/or layout, they might not.
color_maps :: Map Edo (Map MidiNote ColorString)
color_maps = M.fromList $ [
  (41, colors_for_41_edo),
  (50, colors_for_50_edo),
  (53, colors_for_53_edo),

  -- See the comment on the corresponding import statements
  -- for why the lines below differ from the lines above.
  (29, Colors.Edo29.theMap),
  (31, Colors.Edo31.theMap),
  (34, Colors.Edo34.theMap),
  (46, Colors.Edo46.theMap),
  (58, Colors.Edo58.theMap),
  (60, Colors.Edo60.theMap)
  ]

colors_for_31_edo :: Map MidiNote ColorString
colors_for_31_edo = chains_to_note_colors $
                    chains_of_fifths 31 18 2 4

colors_for_41_edo :: Map MidiNote ColorString
colors_for_41_edo = chains_to_note_colors $
                    chains_of_fifths 41 24 4 5

colors_for_50_edo :: Map MidiNote ColorString
colors_for_50_edo = chains_to_note_colors $
                    chains_of_fifths 50 29 3 7

colors_for_53_edo :: Map MidiNote ColorString
colors_for_53_edo = chains_to_note_colors $
                    chains_of_fifths 53 31 5 7


-- * Note chains

type NoteChains =
  [ (EdoNote,
     (Int, -- ^ identifies the chain the note is in
      Int -- ^ identifies the note's position in the chain
     ) ) ]

-- | These can actually be chains of anything,
-- not just fifths.
--
-- PITFALL: Determining the arguments to use for
-- `chains_of_fifths` turned out to be easier than I expected,
-- if the edo has a bosanquet layout.
-- The interval to chain is whatever a fifth is in that edo --
-- 24 in 41-edo, for instance.
-- The distance between chains can (I believe) always be
-- set to equal the diagonal distance in the bosanquet layout.
-- For instance, in 31-edo it's 2 halfsteps, because
-- 31-edo's bosanquet layout uses unit vectors 5\31 and 3\31.
-- Doing that seems to perfectly avoid collisions,
-- such that the last argument, the number of chains,
-- can be equal to `div edo 7`.
chains_of_fifths ::
  Edo ->
  EdoNote -> -- ^ the interval to chain
  EdoNote -> -- ^ the distance between chains
  Int -> -- ^ The number of chains. Currently maxes out at 7,
         -- because that's the number of non-black colors
         -- defined in `colors`. Could easily be more.
  NoteChains
chains_of_fifths edo perfect_fifth jump nChains =
  [ (mod (jump * i + j * perfect_fifth) edo,
     (i, j))
  | i <- [0..nChains-1],
    j <- [0..6] -- To resemble the white keys, i takes 7 values.
                -- If the groups are to have some other size,
                -- this should be a variable argument.
  ]

-- | This turns out not even to be necessary if the edo
-- has a bosanquet layout, although it's a reassuring check.
chains_do_not_overlap :: NoteChains -> Bool
chains_do_not_overlap chains =
  let notes = map fst chains
  in length (S.toList $ S.fromList notes)
     == length notes

chains_to_note_colors :: NoteChains -> Map MidiNote ColorString
chains_to_note_colors = M.fromList . map f where
  f :: (EdoNote, (Int,Int)) -> (MidiNote, ColorString)
  f (note, (group, position)) =
    (note, if position == 0
           then color_white
           else maybe color_black id $
                M.lookup group colors )
