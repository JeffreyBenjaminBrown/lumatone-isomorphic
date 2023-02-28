module Test where

import           Data.Map (Map)
import qualified Data.Map   as M
import           Test.HUnit

import Colors.Wheel
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
  ]

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
  let kd = edoNote_to_keyData 31 12
  assertBool "channel"     $ keyChannel     kd == 0
  assertBool "note"        $ keyNote        kd == 12

  let kd = edoNote_to_keyData 31 (31 + 12)
  assertBool "channel"     $ keyChannel     kd == 1
  assertBool "note"        $ keyNote        kd == 12
