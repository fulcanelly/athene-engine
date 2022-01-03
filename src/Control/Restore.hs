{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
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
import Data.Logic (lobby)
import API.Telegram (ChatId)
import Control.Exception (Exception, catch, throw)

data SavedEvent
  = Intervened Intervention
  | Posted (Maybe AdvPost)
  | Sent
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromRow SavedEvent where
  fromRow = fromJust . decode <$> field

class IsEvent e where
  toEvent :: e -> SavedEvent

instance IsEvent (Maybe AdvPost) where
  toEvent = Posted

instance IsEvent Intervention where
  toEvent = Intervened

setupDB :: HasCallStack => SqlRequest ()
setupDB = do
  execute_ "CREATE TABLE IF NOT EXISTS event_storage(chat, blob, time DATETIME DEFAULT CURRENT_TIMESTAMP)"

addEvent :: Int -> SavedEvent -> SqlRequest ()
addEvent chat event =
  execute "INSERT INTO event_storage(chat, blob) VALUES(?, ?)" (chat, encode event)

addEvent_ :: IsEvent e => Int -> e -> SqlRequest ()
addEvent_ chat event = addEvent chat (toEvent event)

loadState :: ChatId -> SqlRequest [SavedEvent]
loadState chat = query "SELECT blob FROM event_storage WHERE chat = ?" (Only chat)

cleanState :: ChatId -> SqlRequest ()
cleanState chat = execute "DELETE FROM event_storage WHERE chat = ?" (Only chat)

newtype CantRestore = CantRestore String 
  deriving (Show, Exception)

catchRestore :: IO a -> (CantRestore -> IO a) -> IO a
catchRestore = catch

restoreScen :: [SavedEvent] -> Free ScenarioF a -> Free ScenarioF a
restoreScen [] bot = case bot of 
  Free (Eval cmd next) -> restoreScen [] next
  Free (Record next) -> restoreScen [] next
  _ -> bot 


-- todo: make better restoring by utilizing binary search  
-- ie if can't restore: load 1/2 * count and try again 
-- if can then load 1/2 + 1/4 else 1/2 - 1/4 and try restore ...

restoreScen whole @(e : rest) bot = case bot of
  Pure a -> throw $ CantRestore "can't be pure"
  Free sf -> case sf of 
    Expect f -> do 
      let (Intervened (Update u)) = e in case f u of
        Nothing -> throw $ CantRestore "???"
        Just fr -> restoreScen rest fr 
    
    Eval com fr -> do 
      case com of
        SendWith me -> restoreScen rest fr
        _ -> restoreScen whole fr
    
    ReturnIf p fr fr' -> throw $ CantRestore "can't be done"
    
    FindRandPost f -> 
      let (Posted post) = e in restoreScen rest (f post) 

    LoadMyPost f -> do
      let (Posted post) = e in restoreScen rest (f post) 

    Record next -> restoreScen whole next