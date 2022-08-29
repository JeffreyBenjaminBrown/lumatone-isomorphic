{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Maybe as Mb
import           Test.HUnit

import Types
import Data


main :: IO ()
main = putStrLn "Hello."


-- ** Tests with no library dependencies

runTests = mapM_ (putStrLn . show) $
  map (\(a,b) -> (a, all id b))
  [ ("test_board_key_to_xy", test_board_key_to_xy)
  , ("test_xy_to_edoNote", test_xy_to_edoNote) ]


-- ** functions

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
                         $ M.lookup k Data.key_xy_map
  in addXY (mulXY b (5,2)) base_xy

test_board_key_to_xy :: [Bool]
test_board_key_to_xy =
  [ board_key_to_xy (0,0) == (0,0)
  , board_key_to_xy (1,0) == (5,2)
  , board_key_to_xy (0,1) == (1,0)
  , board_key_to_xy (0,2) == (0,1) ]

xy_to_edoNote :: EdoNote -> EdoNote -> (X,Y) -> Int
xy_to_edoNote right_step downleft_step (x,y) =
  right_step * x + downleft_step * y

test_xy_to_edoNote :: [Bool]
test_xy_to_edoNote =
  [ xy_to_edoNote 5 3 (5,2) == 31 ] -- 31-edo Bosanquet, first octave

board_edoNotes :: EdoNote -> EdoNote -> Map (Board, Key) EdoNote
board_edoNotes right_step downleft_step =
  let f :: (Board, Key) -> EdoNote
      f = xy_to_edoNote right_step downleft_step
          . board_key_to_xy
  in M.fromList $ map (\bk -> (bk, f bk)) board_keys
