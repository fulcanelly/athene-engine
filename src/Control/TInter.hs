{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Control.TInter where


import Control.Concurrent ( forkIO, ThreadId )
import API.Telegram
    ( answer, answerWithButtons, chatU, ChatId, Token, Update )
import Control.Async ( Task )
import Control.FreeState
    ( Command(SendWith),
      MessageEntry(TextNButtons, mText, buttons),
      ScenarioF(Expect, Eval, ReturnIf, FindRandPost), catchReturn, ReturnE (ReturnE) )
import Data.Maybe ( fromJust, fromMaybe )
import API.Keyboard (textButton)
import qualified Data.Map as M
import Data.Logic ( lobby )
import Control.Monad.Free ( foldFree )
import Control.Exception ( SomeException, catch, throw )
import GHC.Conc (readTVar, atomically, writeTVar)
import Control.Monad ()
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


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


data UpdateOrCommand
    = Update ! Update
    | Stop


data SQLnTasks
    = SQLnTasks {
        conn :: Connection,
        tasks :: TChan Task
    }

data Context
    = Context {
        mailbox :: TChan Update --todo: change to TChan UpdateOrCommand. reason: suspending old chats
        , tokenC :: String
        , sqlTasks :: SQLnTasks
        , throttleTasks :: TChan Task
        , chat :: Int
        , returnTrigger :: Maybe (Update -> Bool)
    }


instance Show Context where
    show x = "<<Context>>"

type ChatData = M.Map ChatId Context

answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do

    let mid = error "no way to get it yet"
    let text = mText entry
    case buttons entry of
        Nothing -> answer tokenC chat text
        Just butns -> answerWithButtons tokenC chat text (toButtons butns)
    pure ()

    where toButtons = map (map textButton)


iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx (Eval cmd next) = do
    case cmd of
        SendWith entry -> answerWith ctx entry
        _ -> error "unimplemented behavior"
    pure next

iterScenarioTg ctx expect @ (Expect pred) = do
    update <- atomically $ readTChan $ mailbox ctx
    case returnTrigger ctx of
        Just isTimeToReturn -> if isTimeToReturn update then throw ReturnE else handle update
        Nothing -> handle update
    where
    handle update = case pred update of
        Just next -> pure next
        Nothing -> iterScenarioTg ctx expect


iterScenarioTg ctx (ReturnIf pred branch falling) = do
    foldFree (iterScenarioTg $ returnContext ctx pred) branch `catchReturn` const handleFalling
    where handleFalling = iterScenarioTg ctx `foldFree` falling

iterScenarioTg ctx (FindRandPost func) = do
    pure undefined

--iterScenarioTg _ _ = error "unimplemented"

returnContext :: Context -> (Update -> Bool) -> Context
returnContext ctx pred = ctx { returnTrigger = Just pred }



startIter :: Context -> IO ()
startIter ctx = foldFree (iterScenarioTg ctx) lobby

newContext :: Token -> SQLnTasks -> Update -> STM Context
newContext token tasks update = Context
    <$> newTChan <*> pure token
    <*> pure tasks <*> newTChan
    <*> pure (fromJust $ chatU update)
    <*> pure Nothing

type ChatRemover = STM ()

removeChatSync :: TVar ChatData -> ChatId -> ChatRemover
removeChatSync tcdata chat = do
    cdata <- readTVar tcdata
    writeTVar tcdata (chat `M.delete` cdata)

handleInterpreterFailure :: ChatRemover -> Context -> SomeException -> IO ()
handleInterpreterFailure chatRem ctx err = do
    atomically chatRem
    answerWith ctx (TextNButtons ("something went wrong\n\n" ++  show err) (Just [["restart"]]))

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
            writeTChan (mailbox ctx) update
            pure Nothing

        Nothing -> do
            ctx <- contextFactory update
            writeTVar source $ (chat `M.insert` ctx) cdata
            pure $ Just (removeChatSync source chat, ctx)

safeHandleUpdateS :: Token -> SQLnTasks -> TVar ChatData -> Update -> IO ()
safeHandleUpdateS token sqlTasks chats update = do
    let contextFactory = newContext token sqlTasks
    print =<< readTVarIO chats
    res <- atomically $ dispatchUpdateS contextFactory chats update
    maybe mempty createScenario res
    where
    createScenario (chatRemover, ctx) = do
        startNewScenario chatRemover ctx
        mempty


setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []