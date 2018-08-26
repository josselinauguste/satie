module Main where

import           System.Random                  ( newStdGen )
import           Euterpea                       ( play )

import           Composer

main :: IO ()
main = do
  gen <- newStdGen
  play $ generateComposition gen
