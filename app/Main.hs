{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Commands
import Control.Monad.State.Strict (runState)
import qualified Data.Map.Lazy as Map (fromList, empty)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Env
import Types

main :: IO ()
main = do
  putStrLn "Welcome to Hamerkop, a simple Social Network simulation."
  env <- initialEnv
  nextCommand env

nextCommand :: Env -> IO ()
nextCommand env@Env{..} = do
  xs <- getLine
  t <- getCurrentTime
  let (out, nextEnv) = runState (eval xs) (env { eTime = t })
  putStrLn out
  nextCommand nextEnv

initialEnv :: IO Env
initialEnv = do
  t <- getCurrentTime
  return $ Env { cmds = commands, users = Map.empty, eTime = t }

commands :: Commands
commands = Map.fromList $ map (\c@(s, _) -> (s, newCommand c)) commands'

commands' :: [(String, ActionRunner)]
commands' = [ ("->", postRunner)
            , ("", readingRunner)
            , ("follows", followsRunner)
            , ("wall", wallRunner)]

newCommand :: (String, ActionRunner) -> Command
newCommand (n, a) = Cmd { cmd = n, runner = a }
