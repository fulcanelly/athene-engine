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
import Control.FreeState (Command, MessageEntry)
import Data.IORef (IORef, newIORef)


type Target = ChatId 

type FromId = ChatId 

data Notification
    = LikeFrom { from :: FromId, target :: Target }
    | None
    deriving Show
    
type OffersCount = Int

data Intervention
    = Update Update
    | AdvOffers OffersCount ChatId 
    | Stop
    deriving (Show, ToJSON, Generic, FromJSON, Eq)

data SharedState = SharedState {
        tasks :: SQLnTasks
        , execT :: ReqExecutor
        , notifications :: TChan Notification
        , chatSem :: TSem
    }

notifyAboutLike :: Context -> ChatId -> ChatId -> IO ()
notifyAboutLike ctx from target = atomically do
    notify_ ctx `writeTChan` LikeFrom from target

data Context
    = Context {
        mailbox :: TChan Intervention
        
        , execT_ :: ReqExecutor
        
        , sqlTasks :: SQLnTasks
        , throttleTasks :: TChan Task
        , chat :: Int
        , returnTrigger :: Maybe (Update -> Bool)
        , notify_ :: TChan Notification

        , lastMessage :: Maybe Command
        , lastInteraction :: Maybe Int

        , level :: IORef Int
    }


newContext :: SharedState -> Int -> IO Context
newContext shared chatId = Context
    <$> newTChanIO  <*> pure (execT shared)
    <*> pure (tasks shared) <*> newTChanIO
    <*> pure chatId
    <*> pure Nothing
    <*> pure (notifications shared)
    <*> pure Nothing 
    <*> pure Nothing 
    <*> newIORef 0

type ChatData = Map ChatId Context
