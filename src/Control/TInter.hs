{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Control.TInter where


import Control.Concurrent ( forkIO, ThreadId )
import API.Telegram
    ( answer, answerWithButtons, chatU, ChatId, Token, Update, sendGenericMessageWithArgs )
import Control.Async ( Task, wrapIOToFuture, runAsync, awaitAndThen, awaitIOAndThen )
import Control.FreeState

import Data.Maybe ( fromJust, fromMaybe )
import API.ReplyMarkup (textButton)
import qualified Data.Map as M
import Data.Logic 
import Control.Monad.Free ( foldFree, liftF )
import Control.Exception ( SomeException, catch, throw )
import GHC.Conc (readTVar, atomically, writeTVar)
import Control.Monad (void, forever, join)
import Control.Concurrent.STM
    ( STM,
      TVar,
      atomically,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      newTChan,
      readTChan,
      writeTChan,
      TChan )
import Database.SQLite.Simple
import Data.Posts
import Control.Database hiding (tasks)

import Data.Favorites
import Data.Context
import Control.Notifications
import Control.Concurrent.STM.TSem (waitTSem, signalTSem)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


instance Show Context where
    show x = "<<Context>>"


answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do
    let mid = error "no way to get it yet"
    void $ sendGenericMessageWithArgs execT_ chat (method entry) (args entry)



handleUpdate :: Context -> Update -> ScenarioF a -> IO a
handleUpdate ctx update self @ (Expect pred)= do
    case returnTrigger ctx of
        Just isTimeToReturn -> if isTimeToReturn update then throw ReturnE else handle update
        Nothing -> handle update
    where
    handle update = case pred update of
        Just next -> pure next
        Nothing -> iterScenarioTg ctx self

handleUpdate _ _ _ = error "should run only with ScenarioF being Expect"

iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx @ Context{..} (Eval cmd next) = do
    case cmd of
        SendWith entry -> answerWith ctx entry
        CreatePost post -> sqlTasks `runTransaction`
                createNewPost post
            `awaitIOAndThen` do
                const $ pure ()

        LikePost Post{..} -> do
            void $ sqlTasks `runTransaction` do
                _userId `likePostBy` chat

            (ctx `notifyAboutLike` chat) _userId
        DislikePost Post{..} -> do
            void $ sqlTasks `runTransaction` do
                _userId `dislikePostBy` chat

        UpdatePost post -> sqlTasks `runTransaction`
                    updatePost post
                `awaitIOAndThen` do
                    const $ pure ()
                    
        _ -> error "unimplemented behavior"
    pure next

iterScenarioTg ctx expect @ (Expect pred) = do
    intervention <- atomically $ readTChan $ mailbox ctx
    case intervention of
        Update up -> handleUpdate ctx up expect
        AdvOffers n _ -> do
            let adjusted = onPostLike (liftF expect) n
            foldFree (iterScenarioTg ctx) adjusted
        Stop -> undefined

iterScenarioTg ctx (ReturnIf pred branch falling) = do
    foldFree (iterScenarioTg $ returnContext ctx pred) branch `catchReturn` const handleFalling
    where handleFalling = iterScenarioTg ctx `foldFree` falling

iterScenarioTg Context{..} (FindRandPost func) = do
    sqlTasks `runTransaction` findRandomPostExcluding chat
    `awaitIOAndThen` (pure . func)

iterScenarioTg Context{..} (LoadMyPost func) = do
    sqlTasks `runTransaction` getSpecificAt chat
    `awaitIOAndThen` (pure . func)


returnContext :: Context -> (Update -> Bool) -> Context
returnContext ctx pred = ctx { returnTrigger = Just pred }



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
    answerWith ctx (sendTextNButtonsEntry ("something went wrong\n\n" ++  show err) [["restart"]])

startNewScenario :: Scenario () -> ChatRemover -> Context -> IO ThreadId
startNewScenario scen chatRem ctx = forkIO do
    startIter scen ctx `catchAny` handleInterpreterFailure chatRem ctx


type ContextMaker = ChatId -> STM Context

dispatchUpdateS :: ContextMaker
    -> TVar ChatData
    -> Update
    -> STM (Maybe (ChatRemover, Context))
dispatchUpdateS contextFactory source update = do
    cdata <- readTVar source
    let chat = fromJust $ chatU update

    case chat `M.lookup` cdata of
        Just ctx -> do
            writeTChan (mailbox ctx) (Update update)
            pure Nothing

        Nothing -> do
            ctx <- contextFactory (fromJust $ chatU update)
            writeTVar source $ (chat `M.insert` ctx) cdata
            pure $ Just (removeChatSync source chat, ctx)


safeHandleUpdateS :: ContextMaker-> TVar ChatData -> Update -> IO ()
safeHandleUpdateS factory chats update = do
    print =<< readTVarIO chats
    res <- atomically $ dispatchUpdateS factory chats update
    maybe mempty createScenario res
    where
    createScenario (chatRemover, ctx) = do
        startNewScenario lobby chatRemover ctx
        mempty

handleAdvOffer :: ContextMaker -> Intervention -> TVar ChatData -> STM (IO())
handleAdvOffer factory (AdvOffers count chat) cdata = do
    cdata_ <- readTVar cdata

    case chat `M.lookup` cdata_ of
        Just ctx -> do
            writeTChan (mailbox ctx) (AdvOffers count chat)
            pure $ pure ()
        Nothing -> do
            ctx <- factory chat
            writeTVar cdata $ (chat `M.insert` ctx) cdata_
            pure do
                void $ startNewScenario (startOnPostLike count)(removeChatSync cdata chat) ctx

  
handleAdvOffer _ _ _ = error "should be called only with AdvOffers"

handleAll :: SharedState -> TVar ChatData -> TChan Intervention -> IO ()
handleAll state @SharedState{..} chats income = do
    let contextFactory = newContext state

    void $ forkIO $ forever do
        intervention <- atomically do
            waitTSem chatSem
            readTChan income

        print intervention
        case intervention of
            Update up -> safeHandleUpdateS contextFactory chats up
            AdvOffers _ _ -> join $ atomically $ handleAdvOffer contextFactory intervention chats
            Stop -> undefined

        atomically $ signalTSem chatSem


setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []