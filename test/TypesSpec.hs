module TypesSpec (spec) where

import Helper
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Types.toUserAction" $ do

    it "should return a UserAction with all values set" $ do
      toUserAction ["a", "->", "b"] midnight `shouldBe` newUserAct "a" "->" "b" midnight

    it "should return a UserAction with username and time only" $ do
      toUserAction ["a"] midnight `shouldBe` newUserAct "a" "" "" midnight

    it "should return a UserAction without args" $ do
      toUserAction ["a", "->"] midnight `shouldBe` newUserAct "a" "->" "" midnight

  describe "Types.toSystemAction" $ do

    it "should consider only first word from command" $ do
      toSystemAction ["help", "wanted"] midnight `shouldBe` newSysAct "help" midnight
