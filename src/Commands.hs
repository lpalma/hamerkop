{-# LANGUAGE RecordWildCards #-}

module Commands
  ( postRunner
  , readingRunner
  , followsRunner
  , wallRunner
  , helpRunner
  ) where

import Control.Monad.State.Strict
import Data.List (union, sortBy)
import qualified Data.Map.Lazy as Map (alter, lookup, elems)
import Data.DateTime (diffMinutes, diffSeconds)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime)
import Env (findUsers)
import Types

postRunner :: ActionRunner
postRunner a@UserAct{..} = do
  modify $ upsert a
  return $ userName ++ ": " ++ args ++ "\n"

readingRunner :: ActionRunner
readingRunner a@UserAct{..} = gets $ unlines . userPosts userName

followsRunner :: ActionRunner
followsRunner a@UserAct{..} = do
  modify $ newFollow a
  return $ userName ++ " is now following " ++ args ++ "\n"

wallRunner :: ActionRunner
wallRunner a@UserAct{..} = gets $ unlines . userWall userName

helpRunner :: ActionRunner
helpRunner _ = gets $ unlines . (header ++) . cmdDescriptions
  where header = [ "Hamerkop is project that simulates Social Networks behaviour"
                 , "It uses in-memory storage, so all data will be lost upon exit"
                 , "Users will be created when posting their first message"
                 , "" ]

cmdDescriptions :: Env -> [String]
cmdDescriptions e = "Available Commands: \n" : map formatDesc (Map.elems $ cmds e)

formatDesc :: Command -> String
formatDesc Cmd{..} = name ++ desc
  where name = fixName ++ drop (length fixName) "          "
        fixName = if null cmd then "show" else cmd

userWall :: String -> Env -> [String]
userWall n Env{..} =
  case Map.lookup n users of
    Nothing -> ["Oops! " ++ n ++ "'s wall seems empty."]
    Just u@User{..} -> map formatWall $ sortPosts u
  where sortPosts u = sortBy (flip $ comparing date) (wall u)
        wall u = concatMap posts $ u : Map.elems (findUsers (followings u) users)
        formatWall p@Post{..} = user ++ ": " ++ formatPost eTime p

newFollow :: Action -> Env -> Env
newFollow UserAct{..} e@Env{..} = e { users = Map.alter follow userName users }
  where follow Nothing = Just $ newUser (userName, [], [following])
        follow (Just u@User{..}) = Just u { followings = [following] `union` followings }
        following = head $ words args

userPosts :: String -> Env -> [String]
userPosts name e@Env{..} =
  case Map.lookup name users of
    Nothing -> [name ++ " hasn't posted anything yet."]
    Just User{..} -> map (formatPost eTime) posts

formatPost :: UTCTime -> Post -> String
formatPost time Post{..} = msg ++ " (" ++ formatTime date time ++ ")"

formatTime :: UTCTime -> UTCTime -> String
formatTime post env
  | diff <= 1 = "just now"
  | diff < 60 = show diff ++ " seconds ago"
  | diff <= 119 = "1 minute ago"
  | otherwise = show (diffMinutes env post) ++ " minutes ago"
  where diff = diffSeconds env post

upsert :: Action -> Env -> Env
upsert a@UserAct{..} e@Env{..} = e { users = Map.alter upsert' userName users }
  where upsert' Nothing = Just $ newUser (userName, [newPost a], [])
        upsert' (Just u@User{..}) = Just $ addPost a u

newUser :: (String, [Post], [String]) -> User
newUser (n, ps, fs) = User { name = n
                           , posts = ps
                           , followings = fs }

addPost :: Action -> User -> User
addPost a u@User{..} = u { posts = newPost a : posts}

newPost :: Action -> Post
newPost UserAct{..} = Post { user = userName, msg = args, date = time }
