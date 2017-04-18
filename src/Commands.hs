{-# LANGUAGE RecordWildCards #-}

module Commands
  ( postRunner
  , readingRunner
  ) where

import Control.Monad.State.Strict
import Data.List (union)
import qualified Data.Map.Lazy as Map (alter, lookup)
import Data.DateTime (diffMinutes, diffSeconds)
import Data.Time.Clock (UTCTime)
import Types

postRunner :: ActionRunner
postRunner a@UserAct{..} = do
  modify $ upsert a
  return $ userName ++ ": " ++ args

readingRunner :: ActionRunner
readingRunner a@UserAct{..} = gets $ unlines . userPosts userName

userPosts :: String -> Env -> [String]
userPosts name e@Env{..} =
  case Map.lookup name users of
    Nothing -> [name ++ " hasn't posted anything yet."]
    Just User{..} -> map (formatPost eTime) posts

formatPost :: UTCTime -> Post -> String
formatPost time Post{..} = msg ++ " (" ++ (formatTime date time) ++ ")"

formatTime :: UTCTime -> UTCTime -> String
formatTime post env
  | diff <= 1 = "just now"
  | diff < 60 = show diff ++ " seconds ago"
  | diff <= 119 = "1 minute ago"
  | otherwise = show (diffMinutes post env) ++ " minutes ago"
  where diff = diffSeconds post env

upsert :: Action -> Env -> Env
upsert a@UserAct{..} e@Env{..} = e { users = Map.alter upsert' userName users }
  where upsert' Nothing = Just $ newUser (userName, [newPost a], [])
        upsert' (Just u@User{..}) = Just $ addPost a u

newUser :: (String, [Post], [String]) -> User
newUser (n, ps, fs) = User { name = n
                           , posts = ps
                           , followers = []
                           , followings = fs }

addPost :: Action -> User -> User
addPost a u@User{..} = u { posts = newPost a : posts}

newPost :: Action -> Post
newPost UserAct{..} = Post { user = userName, msg = args, date = time }
