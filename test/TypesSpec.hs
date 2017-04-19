module TypesSpec (spec) where

import Helper
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Types.toUserAction" $ do

    it "should return a UserAction with all values set" $
      toUserAction ["a", "->", "b"] midnight `shouldBe` newUserAct "a" "->" "b" midnight

    it "should return a UserAction with username and time only" $
      toUserAction ["a"] midnight `shouldBe` newUserAct "a" "" "" midnight

    it "should return a UserAction without args" $
      toUserAction ["a", "->"] midnight `shouldBe` newUserAct "a" "->" "" midnight

  describe "Types.toSystemAction" $

    it "should consider only first word from command" $
      toSystemAction ["help", "wanted"] midnight `shouldBe` newSysAct "help" midnight
