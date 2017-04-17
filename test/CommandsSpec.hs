module CommandsSpec (spec) where

import Commands (postRunner)
import Control.Monad.State.Strict (evalState, execState)
import qualified Data.Map.Lazy as Map (member, elems)
import Env
import Helper
import Test.Hspec
import Types

spec :: Spec
spec =
  describe "Commands.postRunner" $ do
    
    it "should return the message formatted with the user name" $
      evalState (postRunner postAction) emptyEnv `shouldBe` "foo: hello world!"

    it "should insert new user upon first post" $ do
      let env = execState (postRunner postAction) emptyEnv
      Map.member "foo" (users env) `shouldBe` True

    it "should add a new post to existing user" $ do
      let env = execState (postRunner postAction) singleUserEnv
      let user = head $ Map.elems $ users env
      length (posts user) `shouldBe` 2

singleUserEnv :: Env
singleUserEnv = addUser stubUser emptyEnv

stubUser :: User
stubUser = User
           { name = "foo"
           , posts = [stubPost]
           , followers = []
           , followings = [] }

stubPost :: Post
stubPost = Post
           { user = "foo"
           , msg = "all your base are belong to us."
           , date = midnight }

postAction :: Action
postAction = UserAct
             { userName = "foo"
             , action = "post"
             , args = "hello world!"
             , time = midnight }
