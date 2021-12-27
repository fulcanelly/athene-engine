{-# LANGUAGE BlockArguments #-}
module Data.Context where
import API.Telegram
import Control.Concurrent.STM
import Control.Database hiding (tasks)
import Control.Async
import Data.Maybe
import Data.Map 

data Notification
    = LikeFrom ChatId
    | None

data Intervention
    = Update ! Update
    | AdvOffers Int
    | Stop
    

data SharedState = SharedState {
        tasks :: SQLnTasks
        , token_ :: String
        , notifications :: TChan Notification
    }

notifyAboutLike :: Context -> ChatId -> IO ()
notifyAboutLike ctx chat = atomically do
    notify_ ctx `writeTChan` LikeFrom chat


data Context
    = Context {
        mailbox :: TChan Intervention
        , tokenC :: String
        , sqlTasks :: SQLnTasks
        , throttleTasks :: TChan Task
        , chat :: Int
        , returnTrigger :: Maybe (Update -> Bool)
        , notify_ :: TChan Notification
    }


newContext :: SharedState -> Update -> STM Context
newContext shared update = Context
    <$> newTChan <*> pure (token_ shared)
    <*> pure (tasks shared) <*> newTChan
    <*> pure (fromJust $ chatU update)
    <*> pure Nothing
    <*> pure (notifications shared)

type ChatData = Map ChatId Context
