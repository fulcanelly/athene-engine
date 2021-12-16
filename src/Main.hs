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

import Control.Concurrent
import Network.HTTP ( getRequest, getResponseBody, simpleHTTP )
--import qualified Network.HTTP.Simple as H
--import qualified Network.HTTP.Simple as H
import Control.Monad (forever, join)
import qualified Data.String
import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import API.Telegram
import Data.Maybe
import Control.FreeState
import qualified API.Telegram as T

import API.Keyboard
import Control.FreeState
import Data.Logic
import Data.Generics.Labels
import Control.Lens
import GHC.Stack
import Control.TInter




main :: HasCallStack => IO ()
main = do
    cdata <- setupChatData
    let handler = dispatchUpdate_ token cdata
    forAllUpdates token handler Nothing
    pure ()

