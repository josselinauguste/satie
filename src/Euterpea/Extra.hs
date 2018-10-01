module Euterpea.Extra
  ( lineToPitches
  )
where

import           Data.Maybe                     ( catMaybes )
import           Euterpea.Music

lineToPitches :: Music Pitch -> [Pitch]
lineToPitches m = catMaybes $ musicToPitch <$> lineToList m
 where
  musicToPitch (Prim (Note _duration n)) = Just n
  musicToPitch _                         = Nothing
