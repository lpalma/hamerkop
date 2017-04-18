module CommandsSpec (spec) where

import Commands (postRunner)
import Control.Monad.State.Strict (evalState, execState)
import qualified Data.Map.Lazy as Map (member, elems)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env
import Helper
import Test.Hspec
import Types

spec :: Spec
spec =
  describe "Commands.postRunner" $ do
    
    it "should return the message formatted with the user name" $
      evalState (postRunner $ stubAction "") emptyEnv `shouldBe` "foo: hello world!"

    it "should insert new user upon first post" $ do
      let env = execState (postRunner $ stubAction "") emptyEnv
      Map.member "foo" (users env) `shouldBe` True

    it "should add a new post to existing user" $ do
      let env = execState (postRunner $ stubAction "") singleUserEnv
      let user = head $ Map.elems $ users env
      length (posts user) `shouldBe` 2

singleUserEnv :: Env
singleUserEnv = addUser stubUser emptyEnv

multiplePostsEnv :: Env
multiplePostsEnv = addUser (createUser ("foo", stubPosts, [], [])) emptyEnv

secAfterMidnight :: Integer -> UTCTime
secAfterMidnight = UTCTime (fromGregorian 2017 1 1) . secondsToDiffTime

stubUser :: User
stubUser = createUser ("foo", [head stubPosts], [], [])

createUser :: (String, [Post], [String], [String]) -> User
createUser (n, ps, fers, fings) = User
                                  { name = n
                                  , posts = ps
                                  , followers = fers
                                  , followings = fings }

stubPosts :: [Post]
stubPosts = map createPost [ ("foo", "fourth", midnight)
                           , ("foo", "third", secAfterMidnight 59)
                           , ("foo", "second", secAfterMidnight 100)
                           , ("foo", "first", secAfterMidnight 120) ]

createPost :: (String, String, UTCTime) -> Post
createPost (u, m, d) = Post
                       { user = u
                       , msg = m
                       , date = d }

stubAction :: String -> Action
stubAction a = UserAct
               { userName = "foo"
               , action = a
               , args = "hello world!"
               , time = midnight }
