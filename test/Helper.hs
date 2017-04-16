module Helper
  ( emptyEnv
  , newUserAct
  , midnight
  ) where

import Types
import qualified Data.Map.Lazy as Map
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Env

emptyEnv :: Env
emptyEnv = ([], Map.empty, midnight)

newUserAct :: String -> String -> String -> UTCTime -> Action
newUserAct n act args t = UserAct { userName = n
                                  , action = act
                                  , args = args
                                  , time = t }

midnight :: UTCTime
midnight = UTCTime (fromGregorian 2017 1 1) 0
