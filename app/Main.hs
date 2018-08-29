module Main where

import           System.Random                  ( newStdGen )
import           Euterpea

import           Markov                         ( TransitionMatrix )
import           Composer

main :: IO ()
main = do
  gen <- newStdGen
  play $ generateComposition $ MarkovChain (defaultMatrix, gen)

defaultMatrix :: TransitionMatrix PitchClass
defaultMatrix =
  [ (C, [(C, 0.33), (G, 0.67)])
  , (D, [(C, 0.5), (D, 0.25), (G, 0.25)])
  , (E, [(D, 0.375), (E, 0.625)])
  , (F, [(E, 0.5), (F, 0.5)])
  , (G, [(F, 0.6), (G, 0.4)])
  , (A, [(G, 0.5), (A, 0.5)])
  ]
