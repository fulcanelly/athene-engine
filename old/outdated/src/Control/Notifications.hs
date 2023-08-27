{-# LANGUAGE BlockArguments #-}
module Control.Notifications where
import API.Telegram (ChatId)
import Control.Concurrent.STM (newTChanIO, TChan, TVar, readTChan, atomically, readTVar, writeTChan)
import Control.Concurrent (forkIO)
import Data.Context
import Control.Monad




startNotifServiceStub :: TChan Notification -> TChan Intervention -> IO ()
startNotifServiceStub notifs interv = do
    forkIO $ forever do 
        input' <- atomically $ readTChan notifs
        case input' of
            LikeFrom from target -> do
                print input'
                atomically $ writeTChan interv (AdvOffers 1 target)
            None -> pure ()
       
    pure ()

