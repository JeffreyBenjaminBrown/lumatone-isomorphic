{-# LANGUAGE ScopedTypeVariables #-}

module Colors where

import           Data.List.Extra (maximumOn)
import           Data.List (sortOn)
import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Set (Set)
import qualified Data.Set   as S

import Colors.Simple
import Colors.Rainbow
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


board_key_to_edoNote ::
  (Board,Key) -> Map (Board, Key) EdoNote -> EdoNote
board_key_to_edoNote bk = let
  errMsg = "board_key_to_edoNote: (board,key) "
           ++ show bk ++ "not found."
  in maybe (error errMsg) id . M.lookup bk

overlay_color_for_keys_on_board ::
  Edo
  -> (Board,[Key]) -- ^ keys on a board to color
  -> ColorString
  -> Map (Board, Key) EdoNote
  -> Map MidiNote ColorString
overlay_color_for_keys_on_board edo (b,ks) color lumatone =
  M.fromList
  [ ( mod (board_key_to_edoNote (b,k) lumatone) edo,
      color )
  | k <- ks ]


-- * Mapping notes to colors

{- | In @color overlay edo note@, if @note@ is in @overlay@,
then that determines the color. If not, then the edo equivalence class
of the note is found for that edo,
by using @color_maps :: Map Edo (Map MidiNote ColorString)@.

TODO | PITFALL: There's something unnatural about this,
as edo equivalence classes were also used to construct the overlay.
-}
color ::
  Map MidiNote ColorString
  -> Edo
  -> MidiNote
  -> ColorString
color overlay edo note =
  let color_map :: Map MidiNote ColorString
      color_map =
        maybe colors_for_50_edo id $ {- If it's not an Edo I've made a color map for, I just give it the 50-edo color map, since that's got the maximum possible number of note-color pairs (49, given that there are only 7 colors and 7 notes per color) and is empty almost nowhere. -}
        M.lookup edo color_maps
  in maybe default_color id $
     M.lookup note $ M.union overlay color_map

-- | This assigns colors to EDO values.
-- Not every EDO value needs to have a color assigned,
-- and they do not need to be defined in order.
--
-- These colors work out nicely for Bosanquet 41-edo.
-- For some other edo and/or layout, they might not.
color_maps :: Map Edo (Map MidiNote ColorString)
color_maps = M.fromList $ [
  (41, colors_for_41_edo),
  (53, colors_for_53_edo),

  -- experimental
  (27, rainbow 27 16 27 0), -- 16\27 = 3:2
  (29, rainbow 29  5 29 0), -- 6 = 4/3^(1/2). 5 = 9/8.
  (31, rainbow 31  9 31 0), -- 9\31 = 11:9 = (3:2) ^ (1/2)
  (34, rainbow 34  5 34 0), -- 7\34 = (4:3) ^ (1/2) = 247c
                            -- 5\34 = (3:2) ^ (1/4) = 176c
  (41, rainbow 41  8 41 0), -- 12\41 = (3:2) ^ (1/2)
                            -- 8\41 = (3:2) ^ (1/3)
  (46, rainbow 46  9 46 0), -- 9\27 = 8:7 = (3:2)^(1/3)
  (50, rainbow 50 16 25 0), -- 16\50 = 5:4
  (53, rainbow 53 31 53 0), -- 31\53 = 3:2
  --(58, rainbow 58 34 29 0), -- 17\58 = 3:2 ^ (1/2) = 352c
  (58, rainbow 58 34 29 1), -- 17\58 = 3:2 ^ (1/2) = 352c

  -- See the comment on the corresponding import statements
  -- for why the lines below differ from the lines above.
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
