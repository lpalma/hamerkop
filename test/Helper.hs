{-# LANGUAGE RecordWildCards #-}

module Helper
  ( emptyEnv
  , newUserAct
  , midnight
  , stubRunner
  , stubCommand
  , addUser
  , createEnv
  , createUser
  , createPost
  , stubUsers
  , secAfterMidnight
  , usersEnv
  , newSysAct
  ) where

import Types
import qualified Data.Map.Lazy as Map (insert, empty, fromList)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env

emptyEnv :: Env
emptyEnv = createEnv (Map.empty, Map.empty, midnight)

usersEnv :: Env
usersEnv = createEnv (Map.empty, stubUsers, secAfterMidnight 250)

newSysAct :: String -> UTCTime -> Action
newSysAct xs t = SystemAct { sysAct = xs, sysTime = t }

newUserAct :: String -> String -> String -> UTCTime -> Action
newUserAct n act args t = UserAct { userName = n
                                  , action = act
                                  , args = args
                                  , time = t }

createEnv :: (Commands, Users, UTCTime) -> Env
createEnv (c, u, t) = Env
                      { cmds = c
                      , users = u
                      , eTime = t }

midnight :: UTCTime
midnight = UTCTime (fromGregorian 2017 1 1) 0

stubRunner :: ActionRunner
stubRunner a = return ""

stubCommand :: Command
stubCommand = Cmd { cmd = "stub", runner = stubRunner, desc = "a stub command"}

addUser :: User -> Env -> Env
addUser u@User{..} e@Env{..} = e { users = Map.insert name u users}

createUser :: (String, [Post], [String]) -> User
createUser (n, ps, fs) = User
                         { name = n
                         , posts = ps
                         , followings = fs }

createPost :: (String, String, UTCTime) -> Post
createPost (u, m, d) = Post
                       { user = u
                       , msg = m
                       , date = d }

stubUsers :: Users
stubUsers = Map.fromList $ zip ["foo", "bob", "alice"] users
  where users = map createUser [ ("foo", fooPosts, ["bob", "alice"])
                               , ("bob", bobPosts, ["alice"])
                               , ("alice", alicePosts, ["foo"]) ]

fooPosts :: [Post]
fooPosts = map createPost [ ("foo", "ruuun", secAfterMidnight 200)
                            , ("foo", "goooo", secAfterMidnight 210)
                            , ("foo", "get to the choppaaa!", secAfterMidnight 220) ]

bobPosts :: [Post]
bobPosts = map createPost [ ("bob", "hello!", secAfterMidnight 10)
                            , ("bob", "I'm batman", secAfterMidnight 20)
                            , ("bob", "you ok bro?", secAfterMidnight 215) ]

alicePosts :: [Post]
alicePosts = map createPost [ ("alice", "I like dogs", midnight) ]

secAfterMidnight :: Integer -> UTCTime
secAfterMidnight = UTCTime (fromGregorian 2017 1 1) . secondsToDiffTime
