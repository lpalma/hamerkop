{-# LANGUAGE RecordWildCards #-}

module Commands (postRunner) where

import Control.Monad.State.Strict
import qualified Data.Map.Lazy as Map (alter)
import Data.Time.Clock (UTCTime)
import Types

postRunner :: ActionRunner
postRunner a@UserAct{..} = do
  modify $ upsert a
  return $ userName ++ ": " ++ args

upsert :: Action -> Env -> Env
upsert a@UserAct{..} e@Env{..} = e { users = Map.alter upsert' userName users }
  where upsert' Nothing = Just $ newUser a
        upsert' (Just u@User{..}) = Just $ addPost a u

newUser :: Action -> User
newUser a@UserAct{..} = User { name = userName
                             , posts = [newPost a]
                             , followers = []
                             , followings = [] }

addPost :: Action -> User -> User
addPost a u@User{..} = u { posts = newPost a : posts}

newPost :: Action -> Post
newPost UserAct{..} = Post { user = userName, msg = args, date = time }
