{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.EventSafe.TypesSpec
  ( spec
  ) where

import Database.EventSafe.Types

import Test.Hspec

data FakeEvent
  = FE1 String
  | FE2 Int
  | FE3 Bool
  deriving (Eq, Show, Ord)

instance ResourceRef FakeEvent String where
  event `concerns` ref = case event of
                           FE1 ref' -> ref == ref'
                           _        -> False

instance Resource FakeEvent String where
  firstEvent ~(FE1 ref)
    | ref == "nothing" = Nothing
    | otherwise        = Just $ "firstEvent:" ++ ref

  applyEvent ~(FE1 ref) acc
    | ref == "nothing" = Nothing
    | otherwise        = Just $ acc ++ ref

spec :: Spec
spec = do
  let events =
        [ FE1 "One"
        , FE2 9
        , FE1 "Two"
        , FE3 False
        , FE1 "Three"
        ]
      filteredEvents =
        [ FE1 "One"
        , FE1 "Two"
        , FE1 "Three"
        ]

  describe "buildResource" $ do
    it "builds a resource from a list of events" $ do
      buildResource filteredEvents `shouldBe` Just "firstEvent:OneTwoThree"

    it "returns Nothing if there are no events" $ do
      buildResource ([] :: [FakeEvent]) `shouldBe` (Nothing :: Maybe String)

    it "returns Nothing if any step returns Nothing" $ do
      buildResource (FE1 "nothing" : filteredEvents) `shouldBe` (Nothing :: Maybe String)
      buildResource (filteredEvents ++ [FE1 "nothing"]) `shouldBe` (Nothing :: Maybe String)

  describe "getResource" $ do
    it "builds a resource from a filtered list of events" $ do
      getResource events "One" `shouldBe` Just "firstEvent:One"

    it "returns Nothing if the pool is empty" $ do
      getResource ([] :: [FakeEvent]) "One" `shouldBe` (Nothing :: Maybe String)

    it "returns Nothing if no events match the reference" $ do
      getResource events "Four" `shouldBe` (Nothing :: Maybe String)
