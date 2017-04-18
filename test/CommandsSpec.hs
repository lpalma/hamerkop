module CommandsSpec (spec) where

import Commands (postRunner, readingRunner)
import Control.Monad.State.Strict (evalState, execState)
import qualified Data.Map.Lazy as Map (member, elems)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env
import Helper
import Test.Hspec
import Types

spec :: Spec
spec = do
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

  describe "Commands.readingRunner" $ do

    it "should display alt message if user is not found" $ do
      let failMessage = "foo hasn't posted anything yet.\n"
      evalState (readingRunner $ stubAction "" ) emptyEnv `shouldBe` failMessage

    it "should display user posts with relative post time" $ do
      let expected = unlines [ "fourth (just now)"
                             , "third (59 seconds ago)"
                             , "second (1 minute ago)"
                             , "first (2 minutes ago)" ]
      evalState (readingRunner $ stubAction "") multiplePostsEnv `shouldBe` expected

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
