module Data.Context where
import API.Telegram
import Control.Concurrent.STM
import Control.Database hiding (tasks)
import Control.Async
import Data.Maybe
import Data.Map 

data Notification = Notify ChatId String

data Intervention
    = Update ! Update
    | AdvOffers Int
    | Stop
    


data SharedState = SharedState {
        tasks :: SQLnTasks
        , token_ :: String
        , notifications :: TChan Notification
    }




data Context
    = Context {
        mailbox :: TChan Intervention
        , tokenC :: String
        , sqlTasks :: SQLnTasks
        , throttleTasks :: TChan Task
        , chat :: Int
        , returnTrigger :: Maybe (Update -> Bool)
    }


newContext :: SharedState -> Update -> STM Context
newContext shared update = Context
    <$> newTChan <*> pure (token_ shared)
    <*> pure (tasks shared) <*> newTChan
    <*> pure (fromJust $ chatU update)
    <*> pure Nothing


type ChatData = Map ChatId Context
