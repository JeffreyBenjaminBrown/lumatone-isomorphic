module Test where

import           Data.Map (Map)
import qualified Data.Map   as M
import           Test.HUnit

import Colors
import Colors.Rainbow
import Lib
import Types
import Util


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ test_board_key_to_xy
  , test_xy_to_edoNote
  , test_board_edoNotes
  , test_edoNote_to_keyData
  , test_colorUnitFloat_toHex
  , test_pad0
  , test_rainbowColor
  , test_rainbow
  , test_board_key_to_edoNote
  , test_reps_until_entering
  ]

test_reps_until_entering :: Test
test_reps_until_entering = TestCase $ do
  assertBool "Takes 2 reps for a unit of 5 to enter the set [10]." $
    all id [reps_until_entering x [10] 5 == 2 | x <- [31,41,46,48] ]
  assertBool "takes 3 reps for a unit of 10 to enter [1,2] in 31-edo, because 3*10 = 30 is equal to -1 mod 31 and abs (-1) is in [1,2]." $
    reps_until_entering 31 [1,2] 10 == 3

test_board_key_to_edoNote :: Test
test_board_key_to_edoNote = TestCase $ do
  let right_step = 5
      downright_step = 3
      note_shift = 0
      b :: Map (Board, Key) EdoNote
      b = board_edoNotes right_step downright_step note_shift
  -- These tests depend on the particular labeling of the Lumatone geometry
  -- that its makers decided on. See Layout.hs.
  assertBool "" $
    board_key_to_edoNote (2,0) b + right_step ==
    board_key_to_edoNote (2,1) b
  assertBool "" $
    board_key_to_edoNote (2,0) b + downright_step ==
    board_key_to_edoNote (2,2) b

test_rainbow :: Test
test_rainbow = TestCase $ do
  assertBool "" $ rainbow 12 7 3 0
    == M.fromList [ (0,"ff0000")
                  , (7,"0000ff")
                  , (2,"00ff00") ]
  assertBool "" $ rainbow 12 7 3 1
    == M.fromList [ (1,"ff0000")
                  , (8,"0000ff")
                  , (3,"00ff00") ]

test_rainbowColor :: Test
test_rainbowColor = TestCase $ do
  assertBool "" $ rainbowColor (2/3) == "00ff00"
  assertBool "" $ rainbowColor (1/3) == "0000ff"
  assertBool "" $ rainbowColor 0     == "ff0000"
  assertBool "" $ rainbowColor 0 == rainbowColor 1

test_colorUnitFloat_toHex :: Test
test_colorUnitFloat_toHex = TestCase $ do
  assertBool "0 -> 00" $ colorUnitFloat_toHex 0 == "00"
  assertBool "1 -> ff" $ colorUnitFloat_toHex 1 == "ff"

test_pad0 :: Test
test_pad0 = TestCase $ do
  assertBool "" $ pad0 3 "1x" == "01x"

test_board_key_to_xy :: Test
test_board_key_to_xy = TestCase $ do
  assertBool "board 0, key 0" $ Lib.board_key_to_xy (0,0) == (0,0)
  assertBool "board 0, key 1" $ Lib.board_key_to_xy (0,1) == (1,0)
  assertBool "board 0, key 2" $ Lib.board_key_to_xy (0,2) == (0,1)
  assertBool "board 1, key 0" $ Lib.board_key_to_xy (1,0) == (5,2)

test_xy_to_edoNote :: Test
test_xy_to_edoNote = TestCase $ do
  assertBool "31-edo Bosanquet, first octave" $
    Lib.xy_to_edoNote 5 3 0   (5,2) == 31
  assertBool "31-edo Bosanquet, first octave, shifted by 100" $
    Lib.xy_to_edoNote 5 3 100 (5,2) == 131

test_board_edoNotes :: Test
test_board_edoNotes = TestCase $ do
  let luma31 :: Map (Board, Key) EdoNote
      luma31 = board_edoNotes 5 3 0
  assertBool "key 0"         $ M.lookup (0,0) luma31 == Just 0
  assertBool "key 1"         $ M.lookup (0,1) luma31 == Just 5
  assertBool "key 2"         $ M.lookup (0,2) luma31 == Just 3
  assertBool "octave"        $ M.lookup (1,0) luma31 == Just 31
  assertBool "octave + 8\31" $ M.lookup (1,3) luma31 == Just (31 + 8)
  assertBool "2 octaves"     $ M.lookup (2,0) luma31 == Just 62

test_edoNote_to_keyData :: Test
test_edoNote_to_keyData = TestCase $ do
  let kd = edoNote_to_keyData mempty 31 12
  assertBool "channel"     $ keyChannel     kd == 0
  assertBool "note"        $ keyNote        kd == 12

  let kd = edoNote_to_keyData mempty 31 (31 + 12)
  assertBool "channel"     $ keyChannel     kd == 1
  assertBool "note"        $ keyNote        kd == 12
