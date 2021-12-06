{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.State where
import Data.Functor.Identity
import Data.Posts (AdvPostTemplate)


data OneMore a = One a | More a [a]

data Event

data DialogueState =
    Lobby |
    Post |
    Find |
    Review 


type PartialPost = AdvPostTemplate Maybe

data PostCreationState = 
    FillTitle PartialPost |
    FillImage PartialPost |
    FillLink PartialPost

deriving instance Show DialogueState
deriving instance Read DialogueState

class State a where
    save :: a -> String
    restore :: String -> a


instance State DialogueState where
    save = show
    restore = read


loadState = undefined


handleE :: DialogueState -> Event -> IO DialogueState
handleE = undefined



