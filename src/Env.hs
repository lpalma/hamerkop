{-# LANGUAGE RecordWildCards #-}

module Env where

import Control.Monad.State.Strict
import Data.List
import Data.Time.Clock (UTCTime)
import Types

eval :: String -> Env -> (String, Env)
eval cmd e = case parse cmd e of
               Left error -> (error, e)
               Right (action, run) -> run action e

parse :: String -> Env -> Either Error (Action, ActionRunner)
parse "" e = Left ""
parse args e = case cmd of
                 ('@':xs):ys -> matchRunner (toUserAction (xs:ys) (getTime e)) e
                 _ -> Left $ "Command " ++ head cmd ++ " not found."
               where cmd = words args

matchRunner :: Action -> Env -> Either Error (Action, ActionRunner)
matchRunner a@UserAct{..} (cmds, _, _) = case find ((== action) . cmd) cmds of
                                           Nothing -> Left $ "Command " ++ action ++ " not found."
                                           Just Cmd{..} -> Right (a, runner)

getTime :: Env -> UTCTime
getTime (_, _, t) = t
