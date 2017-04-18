{-# LANGUAGE RecordWildCards #-}

module Helper
  ( emptyEnv
  , newUserAct
  , midnight
  , stubRunner
  , stubCommand
  , addUser
  , createEnv
  ) where

import Types
import qualified Data.Map.Lazy as Map (insert, empty)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env

emptyEnv :: Env
emptyEnv = createEnv (Map.empty, Map.empty, midnight)

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
stubCommand = Cmd { cmd = "stub", runner = stubRunner }

addUser :: User -> Env -> Env
addUser u@User{..} e@Env{..} = e { users = Map.insert name u users}
