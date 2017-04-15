module Types where

import Control.Monad.State.Strict
import qualified Data.Map.Lazy as Map
import Data.Time.Clock (UTCTime)

type ActionRunner = Action -> State Env String
type Users = Map.Map String User
type Env = ([Command], Users, UTCTime)
type Error = String

data User = User
            { name :: String
            , posts :: [Post]
            , followers :: [String]
            , followings :: [String] }

data Post = Post { user :: String 
                 , msg :: String
                 , date :: UTCTime }

data Command = Cmd
               { cmd :: String
               , runner :: ActionRunner }

data Action = UserAct
              { userName :: String
              , action :: String
              , args :: String
              , time :: UTCTime }

toUserAction :: [String] -> UTCTime -> Action
toUserAction [x] t = UserAct { userName = x
                             , action = ""
                             , args = ""
                             , time = t }
toUserAction (x:ys:zs) t = UserAct { userName = x
                                   , action = ys
                                   , args = unwords zs
                                   , time = t }
