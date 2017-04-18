module Types
  ( ActionRunner
  , Users
  , Env(..)
  , Error
  , User(..)
  , Post(..)
  , Command(..)
  , Commands
  , Action(..)
  , toUserAction
  , toSystemAction
  ) where

import Control.Monad.State.Strict
import qualified Data.Map.Lazy as Map
import Data.Time.Clock (UTCTime)

type ActionRunner = Action -> State Env String
type Commands = Map.Map String Command
type Users = Map.Map String User
type Error = String

data Env = Env
           { cmds :: Commands
           , users :: Users
           , eTime :: UTCTime }

data User = User
            { name :: String
            , posts :: [Post]
            , followings :: [String] }

data Post = Post { user :: String 
                 , msg :: String
                 , date :: UTCTime }

data Command = Cmd
               { cmd :: String
               , runner :: ActionRunner }

data Action
  = SystemAct
    { sysAct :: String
    , sysTime :: UTCTime }
  | UserAct
    { userName :: String
    , action :: String
    , args :: String
    , time :: UTCTime }
  deriving (Show, Eq)

toSystemAction :: [String] -> UTCTime -> Action
toSystemAction (x:xs) t = SystemAct { sysAct = x
                                    , sysTime = t }

toUserAction :: [String] -> UTCTime -> Action
toUserAction [x] t = UserAct { userName = x
                             , action = ""
                             , args = ""
                             , time = t }
toUserAction (x:ys:zs) t = UserAct { userName = x
                                   , action = ys
                                   , args = unwords zs
                                   , time = t }
