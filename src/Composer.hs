{-# LANGUAGE TupleSections #-}

module Composer
    ( generateComposition
    , Algorithm(..)
    )
where

import           Euterpea.Music
import           System.Random                  ( random
                                                , StdGen
                                                )

import           Markov                         ( TransitionMatrix
                                                , next
                                                )

data Algorithm = MarkovChain (TransitionMatrix PitchClass, StdGen)

generateComposition :: Algorithm -> Music Pitch
generateComposition (MarkovChain (matrix, gen)) =
    line $ map (note sn . (, 3) . fst) $ iterate generateNextPitch
                                                          (C, gen)
  where
    generateNextPitch :: (PitchClass, StdGen) -> (PitchClass, StdGen)
    generateNextPitch (fromPitch, currentGen) =
        let (choice, nextGen) = random currentGen :: (Float, StdGen)
            nextPitch         = next matrix fromPitch choice
        in  case nextPitch of
                Just actualPitch -> (actualPitch, nextGen)
                Nothing          -> (C, nextGen)
