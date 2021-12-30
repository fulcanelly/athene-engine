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

data SavedEvent
  = Intervened Intervention
  | Posted (Maybe AdvPost)
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

loadState :: Int -> SqlRequest [SavedEvent]
loadState chat = query "SELECT blob FROM event_storage WHERE chat = ?" (Only chat)


restoreScen :: [SavedEvent] -> Free ScenarioF a -> Free ScenarioF a
restoreScen [] bot = bot

restoreScen whole @(e : rest) bot = case bot of
  Pure a -> error "can't be pure"
  Free sf -> case sf of 
    Expect f -> do 
      let (Intervened (Update u)) = e in case f u of
        Nothing -> error "whyyy"
        Just fr -> restoreScen rest fr 
    
    Eval com fr -> restoreScen whole fr
    
    ReturnIf p fr fr' -> error "na"
    
    FindRandPost f -> error "na2"
    
    LoadMyPost f -> do
      let (Posted post) = e in  restoreScen rest (f post) 

