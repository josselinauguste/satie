{-# LANGUAGE TupleSections #-}

module Main where

import           System.Random                  ( newStdGen
                                                , StdGen
                                                )
import           System.Environment             ( getArgs )
import qualified Text.XML                      as X
                                                ( readFile
                                                , def
                                                )
import           Euterpea                       ( play )
import           Euterpea.Extra                 ( lineToPitches )
import           Data.Maybe                     ( maybe )

import           Markov                         ( analyse )
import           Composer
import           MusicXML                       ( fromXML
                                                , toMusic
                                                , MusicXML
                                                )

main :: IO ()
main = do
  args <- getArgs
  let musicXMLFile = head args
  musicXMLDocument <- X.readFile X.def musicXMLFile
  gen              <- newStdGen
  play $ generateComposition $ markovChain (fromXML musicXMLDocument) gen

markovChain :: MusicXML -> StdGen -> Algorithm
markovChain m gen = MarkovChain $ (, gen) transitionMatrix
  where transitionMatrix = analyse $ maybe [] lineToPitches (toMusic m)
