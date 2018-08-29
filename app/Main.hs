module Main where

import           System.Random                  ( newStdGen )
import           Euterpea

import           Markov                         ( TransitionMatrix )
import           Composer

main :: IO ()
main = do
  gen <- newStdGen
  play $ generateComposition $ MarkovChain (defaultMatrix, gen)

defaultMatrix :: TransitionMatrix Pitch
defaultMatrix =
  [ ((C, 3), [((C, 3), 0.33), ((G, 3), 0.67)])
  , ((D, 3), [((C, 3), 0.5), ((D, 3), 0.25), ((G, 3), 0.25)])
  , ((E, 3), [((D, 3), 0.375), ((E, 3), 0.625)])
  , ((F, 3), [((E, 3), 0.5), ((F, 3), 0.5)])
  , ((G, 3), [((F, 3), 0.6), ((G, 3), 0.4)])
  , ((A, 3), [((G, 3), 0.5), ((A, 3), 0.5)])
  ]
