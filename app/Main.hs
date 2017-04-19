{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Commands
import Control.Monad.State.Strict (runState)
import qualified Data.Map.Lazy as Map (fromList, empty)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Env
import Types

welcome :: String
welcome = unlines [ "Welcome to Hamerkop, a Social Network simulation."
                  , "Type :help to see the list of available commands."]

main :: IO ()
main = do
  putStrLn $ "\n" ++ welcome
  env <- initialEnv
  nextCommand env

nextCommand :: Env -> IO ()
nextCommand env@Env{..} = do
  xs <- getLine
  t <- getCurrentTime
  let (out, nextEnv) = runState (eval xs) (env { eTime = t })
  putStrLn $ "\n" ++ out
  nextCommand nextEnv

initialEnv :: IO Env
initialEnv = do
  t <- getCurrentTime
  return $ Env { cmds = commands, users = Map.empty, eTime = t }

commands :: Commands
commands = Map.fromList $ map (\c@(s, _, _) -> (s, newCommand c)) commands'

newCommand :: (String, String, ActionRunner) -> Command
newCommand (n, d, a) = Cmd { cmd = n, desc = d, runner = a }

commands' :: [(String, String, ActionRunner)]
commands' =
  [ ( "->"
    , "Post a message for a new or existing User. Usage: <@user> -> <message>"
    , postRunner )
  , ( ""
    , "Displays the User's Posts. Usage: <@user>"
    , readingRunner )
  , ( "follows"
    , "Follows a User. Usage: <@user> follows <user>"
    , followsRunner )
  , ( "wall"
    , "Displays Posts from a User and its followings. Usage: <@user> wall"
    , wallRunner )
  , ( "help"
    , "Displays this help message. Usage: :help"
    , helpRunner ) ]
