{-# LANGUAGE ScopedTypeVariables #-}

module Util where


myPrint :: (Show a, Foldable t) => t a -> IO ()
myPrint = mapM_ $ putStrLn . show

-- | * Add 0s to the left of a string to make it a certain length.
-- Does not shorten the string.
pad0 :: forall a. Integral a => a -> String -> String
pad0 n x = take (fromIntegral n - length x) (cycle "0") ++ x

relativelyPrime :: Int -> Int -> Bool
relativelyPrime modulus spacing =
  elem 1 [modulus, spacing]
  || elem 1 ( fmap
              (flip mod modulus . (*) spacing)
              [1..modulus] )
