module Control.Restore where

import Control.FreeState
import Data.Context
import Control.Monad.Free
import Data.Posts

data SavedEvent
    = Intervened Intervention
    | Posted (Maybe AdvPost)

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