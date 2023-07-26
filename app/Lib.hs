{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import qualified Data.Set   as S
import qualified Data.List  as L
import qualified Data.Map   as M
import           Data.Map (Map)
import           Data.Maybe as Mb

import Colors
import Colors.Simple
import Layout
import Types


-- | Writes all complete (no missing notes)
-- layouts for edos 1 through 60 to disk.
-- Takes a long time. Colors will in many cases be strange,
-- but repeat at the octave.
make_all_layouts :: IO ()
make_all_layouts = do
  let params = [ (edo, right_step, down_right_step)
               | edo <- [1..60],
                 let max_jump = edo `div` 2,
                 right_step <- [1 .. max_jump],
                 down_right_step <- [-max_jump .. max_jump],
                 relativelyPrime right_step down_right_step ]
      go' (edo,rs,drs) = go edo rs drs 0 1 []
  mapM_ go' params

-- | PITFALL: This is BAD for big numbers.
-- But I doubt it matters for my purposes.
-- todo ? speed
relativelyPrime :: Integral a => a -> a -> Bool
relativelyPrime x y =
  elem 1 [x, y]
  || elem 1 ( fmap
              (flip mod x . (*) y)
              [1..x] )

board_keys :: [ (Board, Key) ]
board_keys = [ (b,k)
             | b <- [0..4],
               k <- [0..55] ]

mulXY :: Int -> (X,Y) -> (X,Y)
mulXY i (x,y) = (i*x, i*y)

addXY :: (X,Y) -> (X,Y) -> (X,Y)
addXY (a,b) (c,d) = (a+c, b+d)

board_key_to_xy :: (Board,Key) -> (X,Y)
board_key_to_xy (b,k) =
  let base_xy :: (X,Y) = maybe (error "out of bounds") id
                         $ M.lookup k Layout.key_xy_map
  in addXY (mulXY b (5,2)) base_xy

-- This does not need an Edo argument,
-- because the EdoNote is not provided modulo the Edo.
xy_to_edoNote ::
  EdoNote    -- ^ How many steps rightward on the keyboard
  -> EdoNote -- ^ How many steps down-right on the keyboard
  -> EdoNote -- ^ A nonnegative shift amount for all pitches pitches.
  -> (X,Y) -> Int
xy_to_edoNote right_step downright_step note_shift (x,y) =
  right_step * x + downright_step * y + note_shift

{- | PITFALL: This is unaware of the Edo,
and usually (always?) returns values above it for some keys. -}
board_edoNotes ::
  EdoNote    -- ^ How much pitch changes as one steps right on the keyboard.
  -> EdoNote -- ^ How much pitch changes as one steps down-right.
  -> EdoNote -- ^ A nonnegative shift amount for all pitches.
  -> Map (Board, Key) EdoNote
board_edoNotes right_step downright_step note_shift =
  let f :: (Board, Key) -> EdoNote
      f = xy_to_edoNote right_step downright_step note_shift
          . board_key_to_xy
  in M.fromList $ map (\bk -> (bk, f bk)) board_keys

nonnegative_keyData :: Map (Board, Key) KeyData
                    -> Map (Board, Key) KeyData
nonnegative_keyData l = let
  min_midiChannel :: MidiChannel
  min_midiChannel = minimum $ map keyChannel $ M.elems l
  min_midiNote    :: MidiNote
  min_midiNote    = minimum $ map keyNote    $ M.elems l
  subtract_those :: KeyData -> KeyData
  subtract_those kd = kd {
    -- PITFALL: Add 1 to the MIDI Channel because channels are 1-indexed,
    -- (Keys, by contrast, are 0-indexed like God intended.)
    keyNote    = keyNote    kd - min_midiNote,
    keyChannel = keyChannel kd - min_midiChannel + 1 }
  in M.map subtract_those l

edoNote_to_keyData ::
  Map MidiNote ColorString -> Edo -> EdoNote -> KeyData
edoNote_to_keyData overlay e en = let
  midiNote = mod en e
  in KeyData { keyChannel     = div en e,
               keyNote        = midiNote,
               keyColorString = color overlay e midiNote }

shift_channels ::
  MidiChannel
  -> Map (Board, Key) KeyData
  -> Map (Board, Key) KeyData
shift_channels channel_shift =
  let f :: KeyData -> KeyData
      f kd = kd { keyChannel = keyChannel kd + channel_shift }
  in M.map f

lumatone :: Edo -> EdoNote -> EdoNote -> EdoNote -> MidiChannel
         -> [(ColorString, Board, [Key])]
         -> Map (Board, Key) KeyData
lumatone edo right_step downright_step note_shift channel_shift
         cbks = let
  m_bk_e :: Map (Board,Key) EdoNote
  m_bk_e = board_edoNotes right_step downright_step note_shift
  up_step = right_step - downright_step
  overlay :: Map MidiNote ColorString
  overlay = foldr M.union mempty
    [ overlay_color_for_keys_on_board edo (b,ks) c m_bk_e
    | (c,b,ks) <- cbks ]
  in shift_channels channel_shift
     $ nonnegative_keyData
     $ M.map (edoNote_to_keyData overlay edo) m_bk_e

{- | TODO ? Check, then formalize this test.
l = lumatone 41 7 3
(b,k) = (2,3)
kd = maybe (error "") id $ M.lookup (b,k) l
keyStrings k kd
-}
keyStrings :: Key -> KeyData -> [String]
keyStrings k kd =
  [ "Key_"  ++ show k ++ "=" ++ show (keyNote        kd),
    "Chan_" ++ show k ++ "=" ++ show (keyChannel     kd),
    "Col_"  ++ show k ++ "=" ++ show (keyColorString kd) ]

{- | Returns, I think, a list of 1 + 3*55 keys --
the first being a line to introduce the Board,
and every group of three thereafter corresponding to a key. -}
boardStrings :: Board -> Map (Board,Key) KeyData -> [String]
boardStrings b m = let
  first :: String = "[Board" ++ show b ++ "]"
  rest :: [String] = concat
    [ keyStrings k kd
    | k <- [0..55],
      let kd = maybe (error "boardStrings lookup failed") id
               $ M.lookup (b,k) m ]
  in first : rest

output_path :: Edo -> EdoNote -> EdoNote -> EdoNote -> MidiChannel -> String
output_path edo right_step downright_step note_shift channel_shift =
  "output/" ++ concat
  ( L.intersperse "." -- basename
    $ filter (/= "")
    [ show edo ++ "edo"
    , show right_step ++ "right"
    , show downright_step ++ "downright"
    , if note_shift == 0 then ""
      else "+" ++ show note_shift ++ "notes"
    , if channel_shift == 0 then ""
      else "+" ++ show channel_shift ++ "channels"
    , "ltn" ] )

go :: Edo
   -> EdoNote     -- ^ rightward step
   -> EdoNote     -- ^ down-right step
   -> EdoNote     -- ^ midi note shift
   -> MidiChannel -- ^ midi channel shift
   -> [(ColorString, Board, [Key])]
   -> IO (Map (Board, Key) KeyData)
go edo right_step downright_step note_shift channel_shift
   cbks = do
  let
    l :: Map (Board, Key) KeyData =
      lumatone edo right_step downright_step note_shift channel_shift
               cbks
    file_content :: [String] =
      concat [ boardStrings b l
             | b <- [0..4] ]
    dest_path :: String =
      output_path edo right_step downright_step note_shift channel_shift
  if note_shift < 0 || channel_shift < 0
    then putStrLn $ "WARNING: At least one of the arguments note_shift and channel_shift is < 0. Some values will therefore be negative. The result is (I believe) invalid MIDI. Outputting result despite this madness."
    else return ()
  file_tail :: [String] <-
    lines <$> readFile "data/tail.txt"
  writeFile dest_path $ unlines $ file_content ++ file_tail
  return l
