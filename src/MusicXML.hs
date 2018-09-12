{-# LANGUAGE OverloadedStrings #-}

module MusicXML
  ( fromXML
  , toMusic
  , MusicXML
  )
where

import           Data.Text                      ( unpack )
import           Euterpea.Music
import           Text.XML
import           Text.XML.Cursor

newtype MusicXML = MusicXML Document

fromXML :: Document -> MusicXML
fromXML = MusicXML

toMusic :: MusicXML -> Maybe (Music Pitch)
toMusic (MusicXML document) = line <$> sequence notes
 where
  extractChild n name =
    unpack $ head $ descendant n >>= element name >>= child >>= content
  notes =
    (\n ->
        flip
            note
            ( read $ extractChild n "step" :: PitchClass
            , read $ extractChild n "octave" :: Octave
            )
          <$> toDur (extractChild n "type")
      )
      <$> (descendant (fromDocument document) >>= element "note")

toDur :: String -> Maybe Dur
toDur "whole"   = Just wn
toDur "half"    = Just hn
toDur "quarter" = Just qn
toDur "eight"   = Just en
toDur "16th"    = Just sn
toDur "32nd"    = Just tn
toDur _         = Nothing
