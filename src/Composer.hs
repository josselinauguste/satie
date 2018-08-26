module Composer
    ( generateComposition
    )
where

import           Euterpea.Music
import           System.Random                  ( random
                                                , StdGen
                                                )

import           Markov                         ( TransitionMatrix
                                                , next
                                                )

generateComposition :: StdGen -> Music Pitch
generateComposition gen =
    line $ take duration $ map (note sn . (\n -> (n, 3)) . fst) $ iterate
        generateNextPitch
        (C, gen)
  where
    duration = 16
    generateNextPitch :: (PitchClass, StdGen) -> (PitchClass, StdGen)
    generateNextPitch (fromPitch, currentGen) =
        let (choice, nextGen) = random currentGen :: (Float, StdGen)
            nextPitch         = next matrix fromPitch choice
        in  case nextPitch of
                Just actualPitch -> (actualPitch, nextGen)
                Nothing          -> (C, nextGen)

matrix :: TransitionMatrix PitchClass
matrix =
    [ (C, [(C, 0.33), (G, 0.67)])
    , (D, [(C, 0.5), (D, 0.25), (G, 0.25)])
    , (E, [(D, 0.375), (E, 0.625)])
    , (F, [(E, 0.5), (F, 0.5)])
    , (G, [(F, 0.6), (G, 0.4)])
    , (A, [(G, 0.5), (A, 0.5)])
    ]
