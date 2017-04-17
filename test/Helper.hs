module Helper
  ( emptyEnv
  , newUserAct
  , midnight
  , stubRunner
  , stubCommand
  ) where

import Types
import qualified Data.Map.Lazy as Map
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env

emptyEnv :: Env
emptyEnv = Env { cmds = Map.empty, users = Map.empty, eTime = midnight }

newUserAct :: String -> String -> String -> UTCTime -> Action
newUserAct n act args t = UserAct { userName = n
                                  , action = act
                                  , args = args
                                  , time = t }

midnight :: UTCTime
midnight = UTCTime (fromGregorian 2017 1 1) 0

stubRunner :: ActionRunner
stubRunner a = return ""

stubCommand :: Command
stubCommand = Cmd { cmd = "stub", runner = stubRunner }
