module EnvSpec (spec) where

import Control.Monad.State.Strict
import qualified Data.Map.Lazy as Map (member)
import Env
import Helper
import Test.Hspec
import Types

spec :: Spec
spec =
  describe "Env.eval" $ do
    
    it "should return empty string when empty command is given" $
      evalState (eval "") emptyEnv `shouldBe` ""

    it "should return error message when command is not found" $
      evalState (eval "@john flies away") emptyEnv `shouldBe` "Command flies not found.\n"

    it "user name should start with '@'" $
      evalState (eval "john flies away") emptyEnv `shouldBe` "Command john not found.\n"
