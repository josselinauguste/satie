import           Test.Hspec

import           Markov

main :: IO ()
main = hspec $ describe "markov chain" $ do
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
