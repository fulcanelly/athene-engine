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
import Data.Logic (lobby, onPostLike)
import API.Telegram (ChatId)
import Control.Exception (Exception, catch, throw)
import Text.Pretty.Simple (pPrint)

data SavedEvent
  = Intervened Intervention
  | Posted (Maybe AdvPost)
  | Sent
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

instance FromRow SavedEvent where
  fromRow = fromJust . decode <$> field

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

addEvent :: ChatId -> Level -> SavedEvent -> SqlRequest ()
addEvent chat level event =
  execute "INSERT INTO event_storage(chat, blob, level) VALUES(?, ?, ?)" (chat, encode event, level)

addEvent_ :: IsEvent e => ChatId -> Level -> e -> SqlRequest ()
addEvent_ chat level event = addEvent chat level (toEvent event)

loadState :: ChatId -> SqlRequest [SavedEvent]
loadState chat = query "SELECT blob FROM event_storage WHERE chat = ?" (Only chat)

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


type Restorer p a = [SavedEvent] -> Int -> Free ScenarioF a -> p a

genericRestore :: Restorer a p -> Restorer a p
genericRestore func whole @(e : rest) level bot = case bot of
  Pure a -> throw $ CantRestore "can't be pure"
  Free sf -> case sf of
    Expect f -> do
      let (Intervened int) = e
      case int of
        Update up -> case f up of
          Nothing -> throw $ CantRestore "???"
          Just fr -> func rest level fr

        AdvOffers n i -> do
          let adjusted = onPostLike bot n
          func rest level adjusted
        Stop -> throw $ CantRestore "Not implemented yet"

    Eval com fr -> do
      case com of
        SendWith me -> func rest level fr
        _ -> func whole level fr

    ReturnIf p fr fr' -> throw $ CantRestore "can't be done"

    FindRandPost f ->
      let (Posted post) = e in func rest level (f post)

    LoadMyPost f -> do
      let (Posted post) = e in func rest level (f post)

    Clean level' next -> func whole level' next

genericRestore _ _ _ _ = undefined

-- todo: make better restoring by utilizing binary search  
-- ie if can't restore: load 1/2 * count and try again 
-- if can then load 1/2 + 1/4 else 1/2 - 1/4 and try restore ...


restoreScen :: Restorer Restored a
restoreScen list level bot
  | list == [] =
    case bot of
    Free (Eval cmd next) -> restore [] level next
    Free (Clean _ next) -> restore [] level next
    _ -> Restored bot level
  | otherwise = restore list level bot
  where
  restore = genericRestore restoreScen

restoreScenKeepingLastSent :: Restorer Restored a
restoreScenKeepingLastSent list level bot
  | null list = case bot of
    Free (Clean _ next) -> restore [] level next
    _ -> result

  | all (Sent ==) list = result

  | otherwise = restore list level bot
  where
  result = Restored bot level
  restore = genericRestore restoreScenKeepingLastSent


{-
restoreScenIO list level bot
  | null list = case bot of
    Free (Eval cmd next) -> restoreScenIO [] level next
    Free (Clean _ next) -> restoreScenIO [] level next
    _ -> pure $ Restored bot level
  | otherwise = 
    case list of 
    e : rest -> do
      putStrLn "=== got event: "
      pPrint e
      rest_ list level bot
    _ -> rest_ list level bot

  where
  rest_ = genericRestore restoreScenIO 

-}