module Markov
    ( next
    , analyse
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

analyse :: Eq a => [a] -> TransitionMatrix a
analyse content = map
    (\(f, ts) ->
        let tsLength = fromIntegral $ length ts
        in  (f, map (\(t, c) -> (t, fromIntegral c / tsLength)) ts)
    )
    transitions
  where
    alter :: Eq k => (a -> a) -> k -> a -> [(k, a)] -> [(k, a)]
    alter f k d list =
        let go (x : xs) r found
                | (xk, xv) <- x, xk == k = go xs ((xk, f xv) : r) True
                | otherwise              = go xs (x : r) found
            go [] r True  = r
            go [] r False = (k, d) : r
        in  go list [] False
    accumulateTransitions (from, to) =
        alter (alter (+ (1 :: Integer)) to 1) from [(to, 1)]
    transitions = foldr accumulateTransitions [] (content `zip` tail content)
