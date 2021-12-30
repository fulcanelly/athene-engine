{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Control.TInter where

import API.ReplyMarkup (textButton)
import API.Telegram
  ( ChatId,
    Token,
    Update,
    answer,
    answerWithButtons,
    chatU,
    sendGenericMessageWithArgs,
  )
import Control.Async ( awaitIO, awaitIOAndThen )
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TVar,
    atomically,
    newTChan,
    newTVarIO,
    readTChan,
    readTVar,
    readTVarIO,
    writeTChan,
    writeTVar,
  )
import Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import Control.Database ( runTransaction )
import Control.Exception (SomeException, catch, throw)
import Control.FreeState
    ( catchReturn,
      sendText,
      Command(UpdatePost, SendWith, CreatePost, LikePost, DislikePost),
      MessageEntry(method, args),
      ReturnE(ReturnE),
      Scenario,
      ScenarioF(..) )
import Control.Monad (forever, join, void)
import Control.Monad.Free (foldFree, liftF)
import Control.Notifications ()
import Control.Restore ( addEvent_, loadState, restoreScen, IsEvent )
import Data.Context
    ( newContext,
      notifyAboutLike,
      ChatData,
      Context(..),
      Intervention(..),
      SharedState(..) )
import Data.Favorites ( dislikePostBy, likePostBy )
import Data.Logic ( lobby, onPostLike, startOnPostLike )
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Posts
    ( createNewPost,
      findRandomPostExcluding,
      getSpecificAt,
      updatePost,
      AdvPost(Post, _link, _fileId, _userId, _title) )
import Database.SQLite.Simple ()
import GHC.Conc (atomically, readTVar, writeTVar)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

instance Show Context where
  show x = "<<Context>>"

answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do
  let mid = error "no way to get it yet"
  void $ sendGenericMessageWithArgs execT_ chat (method entry) (args entry)

handleUpdate :: Context -> Update -> ScenarioF a -> IO a
handleUpdate ctx update self@(Expect pred) = do
  case returnTrigger ctx of
    Just isTimeToReturn -> if isTimeToReturn update then throw ReturnE else handle update
    Nothing -> handle update
  where
    handle update = case pred update of
      Just next -> pure next
      Nothing -> iterScenarioTg ctx self
handleUpdate _ _ _ = error "should run only with ScenarioF being Expect"

iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx@Context {..} scen = 
  case scen of
  Eval cmd next -> do
    case cmd of
      SendWith entry -> answerWith ctx entry
      CreatePost post ->
        awaitIO $ sqlTasks `runTransaction` createNewPost post
         
      LikePost Post {..} -> do
        void $ sqlTasks `runTransaction` do _userId `likePostBy` chat
        (ctx `notifyAboutLike` chat) _userId

      DislikePost Post {..} -> do
        void $
          sqlTasks `runTransaction` do
            _userId `dislikePostBy` chat
      UpdatePost post ->
        void $ awaitIO $ sqlTasks `runTransaction` updatePost post

      _ -> error "unimplemented behavior"
    pure next

  expect @(Expect pred) ->  do
    intervention <- atomically $ readTChan mailbox
    sqlTasks `runTransaction` do chat `addEvent_` intervention

    case intervention of
      Update up -> handleUpdate ctx up expect
      AdvOffers n _ -> do
        let adjusted = onPostLike (liftF expect) n
        foldFree (iterScenarioTg ctx) adjusted
      _ -> error "not implemented"

  ReturnIf pred branch falling -> 
    foldFree (iterScenarioTg $ returnContext ctx pred) branch `catchReturn` const handleFalling
    where
      handleFalling = iterScenarioTg ctx `foldFree` falling

  FindRandPost func -> 
    sqlTasks `runTransaction` findRandomPostExcluding chat `awaitIOAndThen` do storeEventAndApply func

  LoadMyPost func -> do
    sqlTasks `runTransaction` getSpecificAt chat `awaitIOAndThen` do storeEventAndApply func

  where 
    storeEventAndApply :: IsEvent e => (e -> b) -> e -> IO b 
    storeEventAndApply func event = do 
        sqlTasks `runTransaction` do chat `addEvent_` event
        pure $ func event
        
returnContext :: Context -> (Update -> Bool) -> Context
returnContext ctx pred = ctx {returnTrigger = Just pred}

startIter :: Scenario () -> Context -> IO ()
startIter sc ctx = foldFree (iterScenarioTg ctx) sc

type ChatRemover = STM ()

removeChatSync :: TVar ChatData -> ChatId -> ChatRemover
removeChatSync tcdata chat = do
  cdata <- readTVar tcdata
  writeTVar tcdata (chat `M.delete` cdata)

handleInterpreterFailure :: ChatRemover -> Context -> SomeException -> IO ()
handleInterpreterFailure chatRem ctx err = do
  atomically chatRem
  answerWith ctx (sendText ("something went wrong\n\n" ++ show err))

startNewScenario :: ChatRemover -> Context -> Scenario () -> IO ThreadId
startNewScenario chatRem ctx scen = forkIO do
  startIter scen ctx `catchAny` handleInterpreterFailure chatRem ctx

type ContextMaker = ChatId -> STM Context

newtype ScenarioStart
  = ScenarioStart (Scenario ())

deliverMail :: ChatRemover -> STM Context -> TVar ChatData -> Intervention -> Scenario () -> IO ()
deliverMail chatRem factory cdata inerv start = do
  cdata_ <- readTVarIO cdata
  let chat = chatOf inerv

  case chat `M.lookup` cdata_ of
    Just ctx -> do
      atomically $ writeTChan (mailbox ctx) inerv
    Nothing -> do
      ctx <- atomically factory
      tasks <- awaitIO $ sqlTasks ctx `runTransaction` loadState chat
      atomically $ writeTVar cdata $ (chat `M.insert` ctx) cdata_

      void $ startNewScenario chatRem ctx if null tasks then start else restoreScen tasks lobby

      deliverMail chatRem factory cdata inerv start

chatOf :: Intervention -> Int
chatOf (Update upd) = fromJust $ chatU upd
chatOf (AdvOffers _ chat) = chat
chatOf _ = undefined

handleAll :: SharedState -> TVar ChatData -> TChan Intervention -> IO ()
handleAll state@SharedState {..} chats income = do
  void $ forkIO $ forever do
    intervention <- atomically do
      waitTSem chatSem
      readTChan income

    let context = newContext state $ chatOf intervention
    let chatRem = removeChatSync chats $ chatOf intervention

    let mailer = deliverMail chatRem context chats intervention

    case intervention of
      Update {} -> mailer lobby
      AdvOffers count _ -> mailer (startOnPostLike count)
      Stop -> putStrLn "stop! -- todo"

    atomically $ signalTSem chatSem

setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []
