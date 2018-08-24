module Composer
    ( playNewComposition
    )
where

import           Euterpea

playNewComposition :: IO ()
playNewComposition = play generateComposition

generateComposition :: Music Pitch
generateComposition = d 3 en
