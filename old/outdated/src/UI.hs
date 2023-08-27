
module UI where

class BotState state where
    nextState :: state -> IO state
    runFor :: state -> idk2 -> IO idk3 -- to rethink 
    save :: state -> IO Bool 

data ChannelAdvPostEntry = Todo

data MutualAdvertsRequest = 
    OnAcceptance |
    Scheduled {
        after :: Int 
    } |
    Cycled {
        period :: Int
      , times :: Int
    }


-- represent possible actions that user can perform 
class UserInterface user where

    -- return starting message
    startBot :: user -> String

    -- creates heading post of the channel or replaces old (it will see other users) 
    aksForPostCreation :: ChannelAdvPostEntry -> user -> IO (Maybe String) 

    showMyProfile :: user -> IO (Maybe ChannelAdvPostEntry)

    -- returns new channel to suggest mutual adverts to (if they are)
    searchNewChannel :: user -> IO (Maybe ChannelAdvPostEntry)

    -- sends request to channel 
    proposeMutualAdvertsExchange :: user -> ChannelAdvPostEntry -> MutualAdvertsRequest -> IO String

    -- gets proposed request 
    getReceivedPropositions :: user -> IO [(ChannelAdvPostEntry, MutualAdvertsRequest)]

    acceptExchangeProposition :: user -> (ChannelAdvPostEntry, MutualAdvertsRequest) -> IO String

    rejectExchangeProposition :: user -> (ChannelAdvPostEntry, MutualAdvertsRequest) -> IO String
