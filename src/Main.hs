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
import API.Telegram
import Data.Maybe
import Control.FreeState
import qualified API.Telegram as T

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

setupDatabase :: IO Connection
setupDatabase = do
    conn <- open "db.sqlite"
    conn `runSql` Post.setupDB 
    conn `runSql` Fav.setupDB 
    pure conn 


main :: HasCallStack => IO ()
main = do
    conn <- setupDatabase
    cdata <- setupChatDataS
    sqlTasks <- SQLnTasks conn <$> initTasks executeAsPossible
    let shared = SharedState sqlTasks token
    let handler = safeHandleUpdateS shared cdata
    forAllUpdates token handler Nothing `finally` do
        close conn

