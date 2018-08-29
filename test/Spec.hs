import           Test.Hspec
import           System.Random                  ( mkStdGen )
import           Euterpea.Music

import           Composer
import           Markov

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

  describe "composer" $ do
    describe "using random generator"
      $             it "generates a suite of notes"
      $             lineToList (generateComposition $ Random $ (mkStdGen 0))
      `shouldNotBe` []
    describe "using markov chain" $ do
      let matrix = [(C, [(C, 0.33), (G, 0.67)])]

      it "composes a song starting with C"
        $ lineToList (generateComposition $ MarkovChain $ (matrix, mkStdGen 0))
        `shouldStartWith` [c 3 sn]
