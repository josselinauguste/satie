import           Test.Hspec
import           System.Random                  ( mkStdGen )
import           Euterpea.Music

import           Composer
import           Markov
import           MusicXML

import           Fixtures.MusicXML              ( minimal
                                                , simpleMelody
                                                )

main :: IO ()
main = hspec $ do
  describe "markov chain" $ do
    it "generates next item" $ do
      let matrix = [('a', [('b', 1)])]
      next matrix 'a' 0.5 `shouldBe` Just 'b'

    it "returns nothing when item has no transition" $ do
      let matrix = [('a', [('b', 1)])]
      next matrix 'b' 0.5 `shouldBe` Nothing

    it "compute next from transition matrix probabilities" $ do
      let matrix = [('a', [('b', 0.5), ('c', 0.5)])]
      next matrix 'a' 0.25 `shouldBe` Just 'b'
      next matrix 'a' 0.75 `shouldBe` Just 'c'

    describe "generating a transition matrix from content analysis" $ do
      it "gets the matrix for the simplest possible content" $ do
        let expectedMatrix = [('a', [('a', 1)])]
        analyse "aa" `shouldBe` expectedMatrix

      it "gets the matrix for a 50/50 content" $ do
        let expectedMatrix = [('a', [('a', 0.5), ('b', 0.5)])]
        analyse "aab" `shouldBe` expectedMatrix

      it "gets the matrix for a more complex content" $ do
        let expectedMatrix =
              [('a', [('c', 1.0)]), ('c', [('c', 0.25), ('a', 0.75)])]
        analyse "ccacaca" `shouldBe` expectedMatrix

  describe "composer" $ do
    describe "using random generator"
      $             it "generates a suite of notes"
      $             lineToList (generateComposition $ Random (mkStdGen 0))
      `shouldNotBe` []
    describe "using markov chain" $ do
      let matrix = [((E, 3), [((C, 3), 0.33), ((G, 3), 0.67)])]

      it "composes a song starting with first note"
        $ lineToList (generateComposition $ MarkovChain (matrix, mkStdGen 0))
        `shouldStartWith` [e 3 sn]

      it "fills non resolved pitches with first pitch"
        $                 do
                            let nonResolvingMatrix = [((E, 3), [((A, 3), 1)])]
                            lineToList
                              ( generateComposition
                              $ MarkovChain (nonResolvingMatrix, mkStdGen 0)
                              )
        `shouldStartWith` [e 3 sn, a 3 sn, e 3 sn]

  describe "parse song" $ do
    it "parses a minimal MusicXML document"
      $          lineToList
      <$>        toMusic minimal
      `shouldBe` Just [c 1 wn]

    it "parses a simple melody "
      $          lineToList
      <$>        toMusic simpleMelody
      `shouldBe` Just [c 4 en, c 4 en, c 4 en, d 4 en, e 4 qn, d 4 qn]
