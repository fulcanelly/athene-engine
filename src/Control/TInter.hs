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
import Control.Async
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
import Control.Restore 

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


instance Show Context where
    show x = "<<Context>>"


answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do
    let mid = error "no way to get it yet"
    void $ sendGenericMessageWithArgs execT_ chat (method entry) (args entry)



handleUpdate :: Context -> Update -> ScenarioF a -> IO a
handleUpdate ctx update self @(Expect pred)= do
    case returnTrigger ctx of
        Just isTimeToReturn -> if isTimeToReturn update then throw ReturnE else handle update
        Nothing -> handle update
    where
    handle update = case pred update of
        Just next -> pure next
        Nothing -> iterScenarioTg ctx self

handleUpdate _ _ _ = error "should run only with ScenarioF being Expect"

iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx @Context{..} (Eval cmd next) = do
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



iterScenarioTg ctx @Context {..} expect @(Expect pred) = do
    intervention <- atomically $ readTChan mailbox
    sqlTasks `runTransaction` do chat `addEvent_` intervention

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
    sqlTasks `runTransaction` findRandomPostExcluding chat `awaitIOAndThen` go
    where
    go post = do
        sqlTasks `runTransaction` do chat `addEvent_` post
        pure $ func post

iterScenarioTg Context{..} (LoadMyPost func) = do
    sqlTasks `runTransaction` getSpecificAt chat `awaitIOAndThen` go
    where
    go post = do
        sqlTasks `runTransaction` do chat `addEvent_` post
        pure $ func post


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
    answerWith ctx (sendText ("something went wrong\n\n" ++  show err))

startNewScenario :: ChatRemover -> Context -> Scenario () -> IO ThreadId
startNewScenario chatRem ctx scen = forkIO do
    startIter scen ctx `catchAny` handleInterpreterFailure chatRem ctx


type ContextMaker = ChatId -> STM Context

newtype ScenarioStart
    = ScenarioStart (Scenario ())

deliverMail :: 
    ChatRemover
    -> STM Context
    -> TVar ChatData
    -> Intervention
    -> Scenario ()
    -> IO ()
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
handleAll state @SharedState {..} chats income = do

    void $ forkIO $ forever do
        intervention <- atomically do
            waitTSem chatSem
            readTChan income


        let context = newContext state $ chatOf intervention
        let chatRem = removeChatSync chats $ chatOf intervention

        putStrLn "got trick!!!!!!343"

        let mailer = deliverMail chatRem context chats intervention
     --   print intervention
        
        case intervention of
            Update {} -> mailer lobby
            AdvOffers count _  -> mailer (startOnPostLike count)
            Stop -> putStrLn "stop!"

        putStrLn "==========+++++!"

        atomically $ signalTSem chatSem


setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []
