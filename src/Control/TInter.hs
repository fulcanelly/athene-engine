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
import Control.Exception (SomeException (SomeException), catch, throw, finally, PatternMatchFail (PatternMatchFail), catches, evaluate)
import Control.FreeState
import Control.Monad (forever, join, void)
import Control.Monad.Free (foldFree, liftF)
import Control.Notifications ()
import Control.Restore 
import Data.Context
    ( newContext,
      notifyAboutLike,
      ChatData,
      Context(..),
      Intervention(..),
      SharedState(..) )
import Data.Favorites
import Data.Logic 
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Posts
import Database.SQLite.Simple ()
import GHC.Conc (atomically, readTVar, writeTVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Restore (Restored(level_))
import Data.Time (getCurrentTime, diffUTCTime)

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
      SendWith entry -> do
        answerWith ctx entry
        level' <- readIORef level
        awaitIO $ sqlTasks `runTransaction` do chat `addEvent` level' $ Sent 
        
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

      DeleteMyPost -> 
        awaitIO $ sqlTasks `runTransaction` do
          deletePost chat
          deletePersonFavorites chat
      _ -> error "unimplemented behavior"
    pure next

  expect @(Expect pred) ->  do
    intervention <- atomically $ readTChan mailbox
    level' <- readIORef level
    sqlTasks `runTransaction` do chat `addEvent_` level' $ intervention

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

  Clean nlevel what -> do
    awaitIO $ sqlTasks `runTransaction` cleanState nlevel chat
    writeIORef level nlevel
    pure what

  where 
    storeEventAndApply :: IsEvent e => (e -> b) -> e -> IO b 
    storeEventAndApply func event = do 
      level' <- readIORef level
      sqlTasks `runTransaction` do chat `addEvent_` level' $ event 
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


tryRestoreStateOrLobby scen tasks chat = evaluate scen
  `catch` \(e :: SomeException) -> do
    putStrLn $ "can't restore state for reason: " <> show e
    awaitIO $ tasks `runTransaction` cleanState 0 chat

    pure $ Restored startBot 0 

deliverMail :: ChatRemover -> STM Context -> TVar ChatData -> Intervention -> IO ()
deliverMail chatRem factory cdata inerv  = do
  cdata_ <- readTVarIO cdata
  let chat = chatOf inerv

  case chat `M.lookup` cdata_ of
    Just ctx -> do
      atomically $ writeTChan (mailbox ctx) inerv
    Nothing -> do
      ctx <- atomically factory
      tasks <- awaitIO $ sqlTasks ctx `runTransaction` loadState chat
      atomically $ writeTVar cdata $ (chat `M.insert` ctx) cdata_

      if null tasks 
        then
          startScen ctx startBot

        else do
          startTime <- getCurrentTime
          
          state <- tryRestoreStateOrLobby (restoreScen tasks 0 startBot) (sqlTasks ctx) chat
          writeIORef (level ctx) (level_ state)
          startScen ctx (scenario state)
          
          diff <- flip diffUTCTime startTime <$> getCurrentTime
          putStrLn $ "restoring took " <> show diff  

      deliverMail chatRem factory cdata inerv 

      where startScen ctx = void . startNewScenario chatRem ctx 

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
    
    ref <- newIORef 0
    let context = newContext ref state $ chatOf intervention
    let chatRem = removeChatSync chats $ chatOf intervention

    deliverMail chatRem context chats intervention

  
    atomically $ signalTSem chatSem

setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []
