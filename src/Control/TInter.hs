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
      ScenarioF(Expect, Eval, ReturnIf), catchReturn )
import Data.Maybe ( fromJust )
import API.Keyboard (textButton)
import qualified Data.Map as M
import Data.Logic ( lobby )
import Control.Monad.Free ( foldFree )
import Control.Exception ( SomeException, catch )
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


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


data UpdateOrCommand
    = Update ! Update
    | Stop

data Context
    = Context {
        mailbox :: TChan Update --todo: change to TChan UpdateOrCommand. reason: suspending old chats
        , tokenC :: String
        , sqlTasks :: TChan Task
        , throttleTasks :: TChan Task
        , chat :: Int
    }
    | ReturningContext {
        inner :: Context
        , pred :: Update -> Bool
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

answerWith ReturningContext{..} entry = answerWith inner entry

iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx (Eval cmd next) = do
    case cmd of
        SendWith entry -> answerWith ctx entry
        _ -> error "unimplemented behavior"
    pure next

iterScenarioTg ctx expect @ (Expect pred) = do
    update <- atomically $ readTChan $ mailbox ctx
    case pred update of
        Just next -> pure next
        Nothing -> iterScenarioTg ctx expect

iterScenarioTg ctx (ReturnIf pred branch falling) = do 
    foldFree (iterScenarioTg $ ReturningContext ctx pred) branch `catchReturn` const handleFalling 
    where handleFalling = iterScenarioTg ctx `foldFree` falling

    
--iterScenarioTg ctx _ = error "unimplemented"


startIter :: Context -> IO ()
startIter ctx = foldFree (iterScenarioTg ctx) lobby

newContext :: String -> Update -> STM Context
newContext token update = Context
    <$> newTChan <*> pure token
    <*> newTChan <*> newTChan <*> pure (fromJust $ chatU update)

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


dispatchUpdateS :: String -> TVar ChatData -> Update -> STM (Either () (IO ()))
dispatchUpdateS token source update = do
    cdata <- readTVar source
    let chat = fromJust $ chatU update

    case chat `M.lookup` cdata of
        Just ctx -> do
            writeTChan (mailbox ctx) update
            pure $ Left ()

        Nothing -> do
            ctx <- newContext token update
            writeTVar source $ (chat `M.insert` ctx) cdata
            pure $ Right do 
                startNewScenario (removeChatSync source chat) ctx
                pure ()
 

safeHandleUpdateS :: Token -> TVar ChatData -> Update -> IO ()
safeHandleUpdateS token chats update = do
    print =<< readTVarIO chats 
    res <- atomically $ dispatchUpdateS token chats update
    either pure Prelude.id res
    


setupChatDataS :: IO (TVar ChatData)
setupChatDataS = newTVarIO []