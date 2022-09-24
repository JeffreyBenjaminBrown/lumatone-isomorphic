{-# LANGUAGE ScopedTypeVariables #-}

module Util where

myPrint :: (Show a, Foldable t) => t a -> IO ()
myPrint = mapM_ $ putStrLn . show
