module Database.EventSafe.DiscPoolSpec
  ( spec
  ) where

import Database.EventSafe.DiscPool

import Test.Hspec

spec :: Spec
spec = do
  describe "planMerges" $ do
    it "returns a list of actions" $ do
      planMerges 2 [IndexItem "filea" 10, IndexItem "fileb" 10, IndexItem "filec" 10, IndexItem "filed" 10] `shouldBe` [[IndexItem "filea" 10, IndexItem "fileb" 10], [IndexItem "filec" 10, IndexItem "filed" 10]]

  describe "allMerges" $ do
    it "returns a list of possible merges" $ do
      allMerges [[1,2,3], [4,5,6], [7,8,9]] `shouldBe`
        [ [[1,2,3,4,5,6], [7,8,9]]
        , [[1,2,3], [4,5,6,7,8,9]]
        ]
