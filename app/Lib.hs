{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Maybe as Mb

import Colors
import Layout
import Types


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
xy_to_edoNote right_step downright_step midi_shift (x,y) =
  right_step * x + downright_step * y + midi_shift

board_edoNotes ::
  EdoNote    -- ^ How much pitch changes as one steps right on the keyboard.
  -> EdoNote -- ^ How much pitch changes as one steps down-right.
  -> EdoNote -- ^ A nonnegative shift amount for all pitches pitches.
  -> Map (Board, Key) EdoNote
board_edoNotes right_step downright_step midi_shift =
  let f :: (Board, Key) -> EdoNote
      f = xy_to_edoNote right_step downright_step midi_shift
          . board_key_to_xy
  in M.fromList $ map (\bk -> (bk, f bk)) board_keys

nonnegative_keyData :: Map (Board, Key) KeyData
                    -> Map (Board, Key) KeyData
nonnegative_keyData l = let
  min_midiChannel :: MidiChannel
  min_midiChannel = minimum $ map keyChannel $ M.elems l
  min_midiNote    :: MidiNote
  min_midiNote    = minimum $ map keyNote    $ M.elems l
  subtract_that :: KeyData -> KeyData
  subtract_that kd = kd {
    -- PITFALL: Add 1 to the MIDI Channel because channels are 1-indexed,
    -- whereas keys are 0-indexed like God intended.
    keyNote    =     keyNote    kd - min_midiNote,
    keyChannel = 1 + keyChannel kd - min_midiChannel }
  in M.map subtract_that l

edoNote_to_keyData :: Edo -> EdoNote -> KeyData
edoNote_to_keyData e en = let
  midiNote =                mod en e
  in KeyData { keyChannel = div en e,
               keyNote    = midiNote,
               keyColor   = color midiNote }

lumatone :: Edo -> EdoNote -> EdoNote -> EdoNote
         -> Map (Board, Key) KeyData
lumatone edo right_step downright_step midi_shift = let
  m_bk_e :: Map (Board,Key) EdoNote
  m_bk_e = board_edoNotes right_step downright_step midi_shift
  in nonnegative_keyData $ M.map (edoNote_to_keyData edo) m_bk_e

-- TODO ? Check, then formalize this test.
-- l = lumatone 41 7 3
-- (b,k) = (2,3)
-- kd = maybe (error "") id $ M.lookup (b,k) l
-- keyStrings k kd
keyStrings :: Key -> KeyData -> [String]
keyStrings k kd =
  [ "Key_"  ++ show k ++ "=" ++ show (keyNote    kd),
    "Chan_" ++ show k ++ "=" ++ show (keyChannel kd),
    "Col_"  ++ show k ++ "=" ++ show (keyColor   kd) ]

boardStrings :: Board -> Map (Board,Key) KeyData -> [String]
boardStrings b m = let
  first :: String = "[Board" ++ show b ++ "]"
  rest :: [String] = concat
    [ keyStrings k kd
    | k <- [0..55],
      let kd = maybe (error "boardStrings lookup failed") id
               $ M.lookup (b,k) m ]
  in first : rest

go :: Edo -> EdoNote -> EdoNote -> EdoNote
   -> IO (Map (Board, Key) KeyData)
go edo right_step downright_step midi_shift = do
  let
    l :: Map (Board, Key) KeyData
    l = lumatone edo right_step downright_step midi_shift
    s :: [String]
    s = concat [ boardStrings b l
               | b <- [0..4] ]
    output_path :: String
    output_path = "output/" ++
                  ( -- basename
                    show edo ++ "edo-" ++
                    show right_step ++ "r-" ++
                    show downright_step ++ "dl" ++
                    ( if midi_shift == 0 then ""
                      else "+" ++ show midi_shift ) ++
                    ".ltn" )
  if midi_shift < 0
    then putStrLn $ "WARNING: midi_Shift (last) argument < 0. Some MIDI pitches will therefore be negative. The result is (I believe) invalid MIDI. Outputting result anyway."
    else return ()
  t :: [String] <-
    lines <$> readFile "data/tail.txt"
  writeFile output_path $ unlines $ s ++ t
  return l
