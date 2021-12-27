{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedLabels #-}
--{-# OPTIONS_GHC -Wall #-}



module Main where

import Data.Posts
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import GHC.Generics
import API.Telegram hiding (Update)
import Data.Maybe
import Control.FreeState
import qualified API.Telegram as T hiding (Update)

import API.ReplyMarkup
import Control.FreeState
import Data.Logic
import Data.Generics.Labels
import Control.Lens
import GHC.Stack
import Control.TInter
import Control.Exception
import qualified Data.Favorites as Fav
import qualified Data.Posts as Post
import Control.Async
import Control.Database
import Data.Context
import Control.Notifications
import Control.Concurrent.STM 
import Control.Concurrent.STM.TSem (newTSem)

setupDatabase :: IO Connection
setupDatabase = do
    conn <- open "db.sqlite"
    conn `runSql` Post.setupDB 
    conn `runSql` Fav.setupDB 
    pure conn 


main :: HasCallStack => IO ()
main = do
    putStrLn "Setting up database"
    conn <- setupDatabase

    cdata <- setupChatDataS
    putStrLn "Spawning sql tasks"
    sqlTasks <- SQLnTasks conn <$> initTasks executeAsPossible

    intervents <- newTChanIO :: IO (TChan Intervention)
    notifs <- newTChanIO :: IO (TChan Notification)

    startNotifServiceStub notifs intervents
    
    sem <- atomically $ newTSem 1
    
    let shared = SharedState sqlTasks token notifs sem
    
    let pushUpdate =
            atomically . writeTChan intervents . Update  

    handleAll shared cdata intervents
 
    forAllUpdates token pushUpdate Nothing `finally` do
        close conn
