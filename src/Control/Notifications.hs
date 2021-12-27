{-# LANGUAGE BlockArguments #-}
module Control.Notifications where
import API.Telegram (ChatId)
import Control.Concurrent.STM (newTChanIO, TChan, TVar)
import Control.Concurrent (forkIO)
import Control.TInter



data Notification = Notify ChatId String

startNotifService :: TVar ChatData -> IO (TChan Notification)
startNotifService cdata = do
    chan <- newTChanIO
    forkIO do 
        pure ()
    pure chan

