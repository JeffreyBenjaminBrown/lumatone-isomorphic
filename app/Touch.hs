-- | FOR DOCUMENTATION: See `data/README.md`.

module Touch where

import Data.List (intersperse)


-- * Exploring curves might be better.

-- | All that seems to matter is (b - a),
-- so this is simpler than `expRange` and achieves the same thing.
expRange' b = let top = exp b
              in (exp (b/2) - 1) / exp b

-- | But this is easier for a human to understand.
expRange a b = let bottom = exp a
                   top = exp b
               in (exp ((a+b)/2) - exp a) / exp b


-- * Generating data

-- | A string like this belongs near the end of any .ltn file.
polynomial_NoteOnOffVelocityCrvTbl_string :: Float -> Float -> String
polynomial_NoteOnOffVelocityCrvTbl_string e c =
  "NoteOnOffVelocityCrvTbl=" ++
  (concat $ intersperse " " $ map show $ polynomial e c)

-- | PURPOSE:
-- Define the `NoteOnOffVelocityCrvTbl` parameter,
-- which is encoded at the end of `data/tail.txt`.
--
-- WHAT IT DOES:
-- See also the excellent comments in the type signature.
-- It generates a list of 128 floats, the first equal to 1,
-- the last equal to 127, rising likea some polynomial function,
-- with a positive second derivative.
-- In `polynomial e c`, `e` is the order of the polynomial,
-- and `c` determines how strong that component is,
-- relative to the linear component.
--
-- HOW TO USE IT:
-- The parameter of most interest, I think, is `e`.
-- `c` just lets you interpolate between a linear curve and "the"
-- polynomial, which is basically invariant as soon (which is's quite soon)
-- as `c` is big enough.
-- For higher values of `e`,
-- the midpoint of the curve can be closer to 0.
-- For instance, if e=2, the midpoint can be no lower than 127/4.
-- (It is higher for low values of `c`.)
polynomial :: Float -- ^ The exponent. A real value greater than 1.
           -> Float -- ^ The scale factor. A real value no less than 0.
           -> [Int] -- ^ A length-128 of integers from 1 to 127.
polynomial e c =
  let f :: Float -> Float
      f x = x + (x*c)**e
      f127 = f 127
  in [ round $ 126 * f x / f127 + 1
     | x <- [0..127] ]


-- * Lumatone Editor-generate data

data LtnFile = LtnFile {
  lumaTouchConfig :: [Int],
  velocityIntrvlTbl :: [Int] }
  deriving (Show, Eq, Ord)

f1 = fifty_pct_flat_then_smoothish_transition
f2 = fifty_pct_nearly_linear_and_low_then_smoothis_concave

-- | The last and best one I made in the Lumatone editor.
fifty_pct_nearly_linear_and_low_then_smoothis_concave :: LtnFile
fifty_pct_nearly_linear_and_low_then_smoothis_concave = LtnFile {
  velocityIntrvlTbl=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 66, 67, 68, 70, 71, 72, 73, 74, 76, 77, 79, 81, 82, 84, 86, 88, 90, 92, 94, 96, 98, 101, 104, 107, 111, 115, 119, 124, 129, 134, 140, 146, 152, 159, 170, 171, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 305, 310],
  lumaTouchConfig=[0, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 15, 16, 16, 17, 17, 18, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 32, 32, 33, 33, 34, 34, 35, 36, 36, 37, 37, 38, 39, 39, 40, 41, 41, 42, 43, 43, 44, 45, 46, 47, 47, 48, 49, 50, 51, 52, 53, 53, 54, 56, 57, 58, 60, 61, 63, 65, 68, 70, 73, 75, 78, 81, 84, 87, 90, 94, 98, 102, 107, 113, 121, 127] }

-- | The second-to-last one I made in the Lumatone editor.
fifty_pct_flat_then_smoothish_transition :: LtnFile
fifty_pct_flat_then_smoothish_transition = LtnFile {
  lumaTouchConfig = [0, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 15, 16, 16, 17, 17, 18, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 32, 32, 33, 33, 34, 34, 35, 36, 36, 37, 37, 38, 39, 39, 40, 41, 41, 42, 43, 43, 44, 45, 46, 47, 47, 48, 49, 50, 51, 52, 53, 53, 54, 56, 57, 58, 60, 61, 63, 65, 68, 70, 73, 75, 78, 81, 84, 87, 90, 94, 98, 102, 107, 113, 121, 127],
  velocityIntrvlTbl = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 66, 67, 68, 70, 71, 72, 73, 74, 76, 77, 79, 81, 82, 84, 86, 88, 90, 92, 94, 96, 98, 101, 104, 107, 111, 115, 119, 124, 129, 134, 140, 146, 152, 159, 170, 171, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 305, 310] }

linear :: LtnFile
linear = LtnFile {
  velocityIntrvlTbl=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 66, 67, 68, 70, 71, 72, 73, 74, 76, 77, 79, 81, 82, 84, 86, 88, 90, 92, 94, 96, 98, 101, 104, 107, 111, 115, 119, 124, 129, 134, 140, 146, 152, 159, 170, 171, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 305, 310],
  lumaTouchConfig=[0, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 15, 16, 16, 17, 17, 18, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 32, 32, 33, 33, 34, 34, 35, 36, 36, 37, 37, 38, 39, 39, 40, 41, 41, 42, 43, 43, 44, 45, 46, 47, 47, 48, 49, 50, 51, 52, 53, 53, 54, 56, 57, 58, 60, 61, 63, 65, 68, 70, 73, 75, 78, 81, 84, 87, 90, 94, 98, 102, 107, 113, 121, 127] }
