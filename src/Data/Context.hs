{-# LANGUAGE BlockArguments #-}
module Data.Context where
import API.Telegram
import Control.Concurrent.STM
import Control.Database hiding (tasks)
import Control.Async
import Data.Maybe
import Data.Map
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Concurrent.STM.TSem (TSem)


type Target = ChatId 

type FromId = ChatId 

data Notification
    = LikeFrom { from :: FromId, target :: Target }
    | None
    deriving Show
    
type OffersCount = Int

data Intervention
    = Update ! Update
    | AdvOffers OffersCount ChatId 
    | Stop
    deriving (Show, ToJSON, Generic, FromJSON)

data SharedState = SharedState {
        tasks :: SQLnTasks
        , token_ :: String
        , notifications :: TChan Notification
        , chatSem :: TSem
    }

notifyAboutLike :: Context -> ChatId -> ChatId -> IO ()
notifyAboutLike ctx from target = atomically do
    notify_ ctx `writeTChan` LikeFrom from target

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


newContext :: SharedState -> ChatId -> STM Context
newContext shared chatId = Context
    <$> newTChan <*> pure (token_ shared)
    <*> pure (tasks shared) <*> newTChan
    <*> pure chatId
    <*> pure Nothing
    <*> pure (notifications shared)

type ChatData = Map ChatId Context
