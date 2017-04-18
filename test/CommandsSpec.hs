{-# LANGUAGE RecordWildCards #-}

module CommandsSpec (spec) where

import Commands (postRunner, readingRunner, followsRunner, wallRunner)
import Control.Monad.State.Strict (evalState, execState)
import qualified Data.Map.Lazy as Map (member, (!), empty)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env
import Helper
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Commands.postRunner" $ do
    
    it "should return the message formatted with the user name" $ do
      let msg = "hello world!"
      evalState (postRunner $ stubAction msg) emptyEnv `shouldBe` "foo: " ++ msg

    it "should insert new user upon first post" $ do
      let env = execState (postRunner $ stubAction "") emptyEnv
      Map.member "foo" (users env) `shouldBe` True

    it "should add a new post to existing user" $ do
      let env = execState (postRunner $ stubAction "") singleUserEnv
      length (posts $ getUser "foo" env) `shouldBe` 2

  describe "Commands.readingRunner" $ do

    it "should display alt message if user is not found" $ do
      let failMessage = "foo hasn't posted anything yet.\n"
      evalState (readingRunner $ stubAction "" ) emptyEnv `shouldBe` failMessage

    it "should display user posts with relative post time" $ do
      let expected = unlines [ "fourth (just now)"
                             , "third (21 seconds ago)"
                             , "second (1 minute ago)"
                             , "first (2 minutes ago)" ]
      evalState (readingRunner $ stubAction "") multiplePostsEnv `shouldBe` expected

  describe "Commands.followRunner" $ do

    it "should update list of followings in the followed user" $ do
      let env = execState (followsRunner $ stubAction "smith") emptyEnv
      elem "smith" (followings $ getUser "foo" env) `shouldBe` True

    it "should consider user name being only first word from argument" $ do
      let env = execState (followsRunner $ stubAction "john smith") emptyEnv
      elem "john" (followings $ getUser "foo" env) `shouldBe` True

  describe "Commands.wallRunner" $ do

    it "should display default message for non created user" $ do
      let m = "Oops! foo's wall seems empty.\n"
      evalState (wallRunner $ stubAction "") emptyEnv `shouldBe` m

    it "should display posts from user and followings sorted by time" $ do
      let m = unlines [ "foo: get to the choppaaa! (31 seconds ago)"
                      , "bob: you ok bro? (35 seconds ago)"
                      , "foo: goooo (40 seconds ago)"
                      , "foo: ruuun (50 seconds ago)"
                      , "bob: I'm batman (3 minutes ago)"
                      , "bob: hello! (4 minutes ago)"
                      , "alice: I like dogs (4 minutes ago)" ]
      evalState (wallRunner $ stubAction "") usersEnv `shouldBe` m

getUser :: String -> Env -> User
getUser n Env{..} = users Map.! n

singleUserEnv :: Env
singleUserEnv = addUser stubUser emptyEnv

multiplePostsEnv :: Env
multiplePostsEnv = addUser (createUser ("foo", stubPosts, [])) env
  where env = createEnv (Map.empty, Map.empty, secAfterMidnight 121)

stubUser :: User
stubUser = createUser ("foo", [head stubPosts], [])

stubPosts :: [Post]
stubPosts = map createPost [ ("foo", "fourth", secAfterMidnight 120)
                           , ("foo", "third", secAfterMidnight 100)
                           , ("foo", "second", secAfterMidnight 59)
                           , ("foo", "first", midnight) ]

stubAction :: String -> Action
stubAction a = UserAct
               { userName = "foo"
               , action = ""
               , args = a
               , time = midnight }
