module EnvSpec (spec) where

import Control.Monad.State.Strict
import Env
import Helper
import Test.Hspec

spec :: Spec
spec = do
  describe "Env.eval" $ do
    
    it "should return empty string when empty command is given" $ do
      evalState (eval "") emptyEnv `shouldBe` ""

    it "should return error message when command is not found" $ do
      evalState (eval "@john flies away") emptyEnv `shouldBe` "Command flies not found."

    it "user name should start with '@'" $ do
      evalState (eval "john flies away") emptyEnv `shouldBe` "Command john not found."
