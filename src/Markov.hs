module Markov
    ( next
    , TransitionMatrix
    , Probability
    )
where

import           Data.List                      ( find )

type Probability a = (a, Float)
type TransitionMatrix a = [(a, [Probability a])]

next :: Eq a => TransitionMatrix a -> a -> Float -> Maybe a
next matrix from roll = lookup from matrix >>= select roll

select :: Eq a => Float -> [Probability a] -> Maybe a
select choice probabilities =
    fst <$> find (\(_n, p) -> p >= choice) cumulativeMatrix
  where
    cumulativeMatrix = foldl (\c p -> c ++ [(fst p, snd p + snd (last c))])
                             [head probabilities]
                             (tail probabilities)
