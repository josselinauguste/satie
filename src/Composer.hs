module Composer
    ( generateComposition
    , Algorithm(..)
    )
where

import           Euterpea.Music
import           System.Random                  ( random
                                                , randomRs
                                                , StdGen
                                                )

import           Markov                         ( TransitionMatrix
                                                , next
                                                )

data Algorithm = Random StdGen
               | MarkovChain (TransitionMatrix Pitch, StdGen)

generateComposition :: Algorithm -> Music Pitch
generateComposition (Random gen) =
    line $ map (note sn . pitch) $ randomRs (30, 80) gen
generateComposition (MarkovChain (matrix, gen)) =
    line $ map (note sn . fst) $ iterate (generateNextPitch firstPitch)
                                         (firstPitch, gen)
  where
    firstPitch = fst $ head matrix
    generateNextPitch :: Pitch -> (Pitch, StdGen) -> (Pitch, StdGen)
    generateNextPitch defaultPitch (fromPitch, currentGen) =
        let (choice, nextGen) = random currentGen :: (Float, StdGen)
            nextPitch         = next matrix fromPitch choice
        in  case nextPitch of
                Just actualPitch -> (actualPitch, nextGen)
                Nothing          -> (defaultPitch, nextGen)
