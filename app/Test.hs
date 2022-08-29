module Test where

import Test.HUnit

import Lib


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ test_board_key_to_xy
  , test_xy_to_edoNote ]

test_board_key_to_xy :: Test
test_board_key_to_xy = TestCase $ do
  assertBool "board 0, key 0" $ Lib.board_key_to_xy (0,0) == (0,0)
  assertBool "board 0, key 1" $ Lib.board_key_to_xy (0,1) == (1,0)
  assertBool "board 0, key 2" $ Lib.board_key_to_xy (0,2) == (0,1)
  assertBool "board 1, key 0" $ Lib.board_key_to_xy (1,0) == (5,2)

test_xy_to_edoNote :: Test
test_xy_to_edoNote = TestCase $ do
  assertBool "31-edo Bosanquet, first octave" $
    Lib.xy_to_edoNote 5 3 (5,2) == 31
