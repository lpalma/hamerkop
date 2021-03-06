{-# LANGUAGE RecordWildCards #-}

module Env
  ( eval
  , findUsers
  ) where

import Control.Monad.State.Strict (State, get, return, modify)
import Data.List (find)
import qualified Data.Map.Lazy as Map (insert, filterWithKey)
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
parse args e@Env{..} =
  case cmd of
    ('@':xs):ys -> matchRunner (toUserAction (xs:ys) eTime) e
    (':':xs):ys -> matchRunner (toSystemAction (xs:ys) eTime) e
    _ -> Left $ "Command " ++ head cmd ++ " not found.\n"
  where cmd = words args

matchRunner :: Action -> Env -> Either Error (Action, ActionRunner)
matchRunner a@SystemAct{..} env = matchRunner' a sysAct env
matchRunner a@UserAct{..} env = matchRunner' a action env

matchRunner' :: Action -> String -> Env -> Either Error (Action, ActionRunner)
matchRunner' a name Env{..} =
  case find ((== name) . cmd) cmds of
    Nothing -> Left $ "Command " ++ name ++ " not found.\n"
    Just Cmd{..} -> Right (a, runner)

findUsers :: [String] -> Users -> Users
findUsers xs = Map.filterWithKey (\k _ -> k `elem` xs)
