module Fixtures.MusicXML
  ( minimal
  , simpleMelody
  )
where

import           Data.ByteString.Lazy.Char8     ( pack )
import           Text.XML                       ( def
                                                , parseLBS_
                                                )


import           MusicXML                       ( MusicXML
                                                , fromXML
                                                )

minimal :: MusicXML
minimal =
  fromString
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
    \<!DOCTYPE score-partwise PUBLIC\
    \  \"-//Recordare//DTD MusicXML 2.0 Partwise//EN\"\
    \  \"http://www.musicxml.org/dtds/partwise.dtd\">\
    \<score-partwise version=\"2.0\">\
    \  <part-list>\
    \    <score-part id=\"P1\">\
    \      <part-name>Minimal</part-name>\
    \    </score-part>\
    \  </part-list>\
    \  <part id=\"P1\">\
    \    <measure number=\"1\">\
    \      <attributes>\
    \        <divisions>1</divisions>\
    \        <key>\
    \          <fifths>0</fifths>\
    \        </key>\
    \        <time>\
    \          <beats>4</beats>\
    \          <beat-type>4</beat-type>\
    \        </time>\
    \        <clef>\
    \          <sign>G</sign>\
    \          <line>2</line>\
    \        </clef>\
    \      </attributes>\
    \      <note>\
    \        <pitch>\
    \          <step>C</step>\
    \          <octave>1</octave>\
    \        </pitch>\
    \        <duration>4</duration>\
    \        <type>whole</type>\
    \      </note>\
    \    </measure>\
    \  </part>\
    \</score-partwise>"

simpleMelody :: MusicXML
simpleMelody =
  fromString
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
    \<!DOCTYPE score-partwise PUBLIC\
    \  \"-//Recordare//DTD MusicXML 2.0 Partwise//EN\"\
    \  \"http://www.musicxml.org/dtds/partwise.dtd\">\
    \<score-partwise version=\"2.0\">\
    \  <part-list>\
    \    <score-part id=\"P1\">\
    \      <part-name>Minimal</part-name>\
    \    </score-part>\
    \  </part-list>\
    \  <part id=\"P1\">\
    \    <measure number=\"1\">\
    \      <attributes>\
    \        <divisions>2</divisions>\
    \        <key>\
    \          <fifths>0</fifths>\
    \          <mode>major</mode>\
    \        </key>\
    \        <time>\
    \          <beats>4</beats>\
    \          <beat-type>4</beat-type>\
    \        </time>\
    \        <clef>\
    \          <sign>G</sign>\
    \          <line>2</line>\
    \        </clef>\
    \      </attributes>\
    \      <note>\
    \        <pitch>\
    \          <step>C</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>1</duration>\
    \        <type>eight</type>\
    \      </note>\
    \      <note>\
    \        <pitch>\
    \          <step>C</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>1</duration>\
    \        <type>eight</type>\
    \      </note>\
    \      <note>\
    \        <pitch>\
    \          <step>C</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>1</duration>\
    \        <type>eight</type>\
    \      </note>\
    \      <note>\
    \        <pitch>\
    \          <step>D</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>1</duration>\
    \        <type>eight</type>\
    \      </note>\
    \      <note>\
    \        <pitch>\
    \          <step>E</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>2</duration>\
    \        <type>quarter</type>\
    \      </note>\
    \      <note>\
    \        <pitch>\
    \          <step>D</step>\
    \          <octave>4</octave>\
    \        </pitch>\
    \        <duration>2</duration>\
    \        <type>quarter</type>\
    \      </note>\
    \    </measure>\
    \  </part>\
    \</score-partwise>"

fromString :: String -> MusicXML
fromString s = fromXML $ parseLBS_ def $ pack s
