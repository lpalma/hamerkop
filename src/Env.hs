{-# LANGUAGE RecordWildCards #-}

module Env (eval) where

import Control.Monad.State.Strict (State, get, return, modify)
import Data.List (find)
import qualified Data.Map.Lazy as Map (insert)
import Data.Time.Clock (UTCTime)
import Types

eval :: String -> State Env String
eval cmd = do
  e <- get
  case parse cmd e of
    Left error -> return error
    Right (action, run) -> run action

parse :: String -> Env -> Either Error (Action, ActionRunner)
parse "" e = Left ""
parse args e@Env{..} = case cmd of
                         ('@':xs):ys -> matchRunner (toUserAction (xs:ys) eTime) e
                         _ -> Left $ "Command " ++ head cmd ++ " not found."
                       where cmd = words args

matchRunner :: Action -> Env -> Either Error (Action, ActionRunner)
matchRunner a@UserAct{..} Env{..} = case find ((== action) . cmd) cmds of
                                      Nothing -> Left $ "Command " ++ action ++ " not found."
                                      Just Cmd{..} -> Right (a, runner)
