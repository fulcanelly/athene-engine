{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module Control.Restore where

import Control.FreeState
import Data.Context
import Control.Monad.Free
import Data.Posts
import Data.Maybe
import Control.Database
import GHC.Generics
import Data.Aeson
import Database.SQLite.Simple hiding (execute_, execute, query)
import GHC.Stack
import Data.Logic (lobby, onPostLike)
import API.Telegram (ChatId)
import Control.Exception (Exception, catch, throw)
import Text.Pretty.Simple (pPrint)
import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Deriving.Aeson

data SavedEvent
  = Intervened Intervention
  | Posted (Maybe AdvPost)
  | Sent
  deriving (Show, Generic, Eq)
  deriving (ToJSON, FromJSON) via (CustomJSON '[SumUntaggedValue ] SavedEvent)

instance FromRow SavedEvent where
  fromRow = fromJust . decode <$> field


type Time = Int

data RawTimedEvent
  = RawTimedEvent { 
    raw :: ByteString
    , time' :: Time
  }

data TimedEvent 
  = TimedEvent { 
    event :: SavedEvent
    , time :: Time 
  }

instance FromRow RawTimedEvent where
  fromRow = RawTimedEvent <$> field <*> field

fromRaw :: RawTimedEvent -> TimedEvent
fromRaw RawTimedEvent {..} = TimedEvent (fromJust $ decode raw) time'

type Level = Int

class IsEvent e where
  toEvent :: e -> SavedEvent

instance IsEvent (Maybe AdvPost) where
  toEvent = Posted

instance IsEvent Intervention where
  toEvent = Intervened

setupDB :: HasCallStack => SqlRequest ()
setupDB = do
  execute_ "CREATE TABLE IF NOT EXISTS event_storage(chat, blob, \
  \ level INTEGER, \
  \ time DATETIME DEFAULT CURRENT_TIMESTAMP)"


loadRawStateWithTime :: ChatId -> SqlRequest [RawTimedEvent]
loadRawStateWithTime chat =  query "SELECT blob, time FROM event_storage WHERE chat = ?" (Only chat)


loadStateWithTime :: ChatId -> SqlRequest [TimedEvent]
loadStateWithTime x = do
  map fromRaw <$> loadRawStateWithTime x

addEvent :: ChatId -> Level -> SavedEvent -> SqlRequest ()
addEvent chat level event =
  execute "INSERT INTO event_storage(chat, blob, level) VALUES(?, ?, ?)" (chat, encode event, level)

addEvent_ :: IsEvent e => ChatId -> Level -> e -> SqlRequest ()
addEvent_ chat level event = addEvent chat level (toEvent event)

loadState :: ChatId -> SqlRequest [SavedEvent]
loadState chat = query "SELECT blob FROM event_storage WHERE chat = ?" (Only chat)

cleanAllAfterMoment :: Time -> ChatId -> SqlRequest ()
cleanAllAfterMoment chat moment = execute "DELETE FROM event_storage WHERE chat = ? AND time >= ?" (chat, moment)

cleanState :: Level -> ChatId -> SqlRequest ()
cleanState level chat = execute "DELETE FROM event_storage WHERE chat = ? AND level >= ?" (chat, level)

newtype CantRestore = CantRestore String 
  deriving (Show, Exception)

catchRestore :: IO a -> (CantRestore -> IO a) -> IO a
catchRestore = catch

data Restored a = Restored {
    scenario :: Scenario a,
    level_ :: Int
  }

restoreScen :: [SavedEvent] -> Level -> Free ScenarioF a -> Restored a
restoreScen [] level bot  = case bot of 
  Free (Eval cmd next) -> restoreScen [] level next
  Free (Clean _ next) -> restoreScen [] level next
  _ -> Restored bot level 


-- todo: make better restoring by utilizing binary search  
-- ie if can't restore: load 1/2 * count and try again 
-- if can then load 1/2 + 1/4 else 1/2 - 1/4 and try restore ...

restoreScen whole @(e : rest) level bot = case bot of
  Pure a -> throw $ CantRestore "can't be pure"
  Free sf -> case sf of 
    Expect f -> do 
      let (Intervened int) = e
      case int of 
        Update up -> case f up of
          Nothing -> throw $ CantRestore "???"
          Just fr -> restoreScen rest level fr 
        AdvOffers n i -> do
          --todo: writeIORef lastBeforeSwitch <$> readIORef lastSend
          let adjusted = onPostLike bot n
          restoreScen rest level adjusted
        Stop -> throw $ CantRestore "Not implemented yet"

    Eval com fr -> do 
      case com of
        SendWith me -> restoreScen rest level fr
        _ -> restoreScen whole level fr
    
    ReturnIf p fr fr' -> throw $ CantRestore "can't be done"
    
    FindRandPost f -> 
      let (Posted post) = e in restoreScen rest level (f post) 

    LoadMyPost f -> do
      let (Posted post) = e in restoreScen rest level (f post) 

    Clean level' next -> restoreScen whole level' next


restoreScenIO [] bot = case bot of 
  Free (Eval cmd next) -> restoreScenIO [] next
  Free (Clean _ next) -> restoreScenIO [] next
  _ -> pure bot 



restoreScenIO whole @(e : rest) bot = do
  putStrLn "=== got event: "
  pPrint e
  case bot of
    Pure a -> throw $ CantRestore "can't be pure"
    Free sf -> case sf of 
      Expect f -> do 
        let (Intervened (Update u)) = e in case f u of
          Nothing -> throw $ CantRestore "???"
          Just fr -> restoreScenIO rest fr 
      
      Eval com fr -> do 
        case com of
          SendWith me -> restoreScenIO rest fr
          _ -> restoreScenIO whole fr
      
      ReturnIf p fr fr' -> throw $ CantRestore "can't be done"
      
      FindRandPost f -> 
        let (Posted post) = e in restoreScenIO rest (f post) 

      LoadMyPost f -> do
        let (Posted post) = e in restoreScenIO rest (f post) 

      Clean _ next -> restoreScenIO whole next
