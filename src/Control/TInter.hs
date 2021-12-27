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
import Data.Logic ( lobby, onPostLike )
import Control.Monad.Free ( foldFree, liftF )
import Control.Exception ( SomeException, catch, throw )
import GHC.Conc (readTVar, atomically, writeTVar)
import Control.Monad (void)
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

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


instance Show Context where
    show x = "<<Context>>"


answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do
    let mid = error "no way to get it yet"
    void $ sendGenericMessageWithArgs tokenC chat (method entry) (args entry)



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

            ctx `notifyAboutLike` chat
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
        AdvOffers n -> do
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



startIter :: Context -> IO ()
startIter ctx = foldFree (iterScenarioTg ctx) lobby



type ChatRemover = STM ()

removeChatSync :: TVar ChatData -> ChatId -> ChatRemover
removeChatSync tcdata chat = do
    cdata <- readTVar tcdata
    writeTVar tcdata (chat `M.delete` cdata)

handleInterpreterFailure :: ChatRemover -> Context -> SomeException -> IO ()
handleInterpreterFailure chatRem ctx err = do
    atomically chatRem
    answerWith ctx (sendTextNButtonsEntry ("something went wrong\n\n" ++  show err) [["restart"]])

startNewScenario :: ChatRemover -> Context -> IO ThreadId
startNewScenario chatRem ctx = forkIO do
    startIter ctx `catchAny` handleInterpreterFailure chatRem ctx

dispatchUpdateS :: (Update -> STM Context)
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
            ctx <- contextFactory update
            writeTVar source $ (chat `M.insert` ctx) cdata
            pure $ Just (removeChatSync source chat, ctx)




safeHandleUpdateS :: SharedState -> TVar ChatData -> Update -> IO ()
safeHandleUpdateS state chats update = do
    let contextFactory = newContext state
    print =<< readTVarIO chats
    res <- atomically $ dispatchUpdateS contextFactory chats update
    maybe mempty createScenario res
    where
    createScenario (chatRemover, ctx) = do
        startNewScenario chatRemover ctx
        mempty


setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []