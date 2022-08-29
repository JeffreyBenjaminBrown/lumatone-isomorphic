{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Data.Map (Map)
import qualified Data.Map   as M
import           Data.Maybe as Mb

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

xy_to_edoNote :: EdoNote -> EdoNote -> (X,Y) -> Int
xy_to_edoNote right_step downleft_step (x,y) =
  right_step * x + downleft_step * y

board_edoNotes ::
  EdoNote -> EdoNote -> Map (Board, Key) EdoNote
board_edoNotes right_step downleft_step =
  let f :: (Board, Key) -> EdoNote
      f = xy_to_edoNote right_step downleft_step
          . board_key_to_xy
  in M.fromList $ map (\bk -> (bk, f bk)) board_keys

-- | To be used on the output of `board_edoNotes`
min_edoNote :: Map (Board, Key) EdoNote -> EdoNote
min_edoNote = minimum . map snd . M.toList
