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

setupDB :: SqlRequest ()
setupDB = do 
    execute_ "CREATE TABLE IF NOT EXISTS event_storage(chat, blob, time DATETIME DEFAULT CURRENT_TIMESTAMP)"

addEvent :: Int -> SavedEvent -> SqlRequest ()
addEvent chat event = 
    execute "INSERT INTO event_storage(chat, blob) VALUES(?, ?)" (chat, encode event)

addEvent_ :: IsEvent e => Int -> e -> SqlRequest ()
addEvent_ chat event = addEvent chat (toEvent event)

loadState :: Int -> SqlRequest [SavedEvent]
loadState chat = query "SELECT * FROM event_storage WHERE chat = ?" (Only chat) 


restoreScen :: [SavedEvent] -> Free ScenarioF a -> Free ScenarioF a
restoreScen [] bot = bot

restoreScen (e : rest) (Free (Eval _ bot)) = restoreScen rest bot

--FindRandPost
restoreScen (e : rest) (Free ReturnIf {}) = error "not supported"

restoreScen (e : rest) (Free (FindRandPost consumer)) = case e of 
        Posted post -> rest `restoreScen` consumer post
        _ -> error "can't restore state"

restoreScen (e : rest) (Free (LoadMyPost consumer)) = case e of 
        Posted post -> rest `restoreScen` consumer post
        _ -> error "can't restore state"

restoreScen _ _ = undefined