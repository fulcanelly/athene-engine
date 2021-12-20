{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}

module Control.TInter where


import Control.Concurrent 
import API.Telegram
import Control.Async ( Task )
import Control.FreeState 
import Data.Maybe
import API.Keyboard (textButton)
import qualified Data.Map as M
import Data.Logic
import Control.Monad.Free
import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


-- * make interpreter interceptor which will log and remove failed tasks

data UpdateOrCommand 
    = Update ! Update 
    | Stop

data Context
    = Context {
        mailbox :: Chan Update
        , tokenC :: String
        , sqlTasks :: Chan Task
        , throttleTasks :: Chan Task
        , chat :: Int
    } 

instance Show Context where
    show x = "<<Context>>"

type ChatData = M.Map ChatId Context

answerWith :: Context -> MessageEntry -> IO ()
answerWith Context {..} entry = do
    --let (Just mid) = msgIdU update
    let mid = error "no way to get it yet"
    let text = mText entry
    case buttons entry of
      Nothing -> answer tokenC chat text
      Just butns -> answerWithButtons tokenC chat text (toButtons butns)
    pure ()

    where toButtons = map (map textButton)  

-- >>
test = pure ""

iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx (Eval cmd next) = do
    case cmd of 
        SendWith entry -> answerWith ctx entry
        _ -> error "unimplemented behavior"   
    pure next

iterScenarioTg ctx expect@ (Expect pred) = do
    update <- readChan $ mailbox ctx
    case pred update of 
        Just next -> pure next
        Nothing -> iterScenarioTg ctx expect
        
iterScenarioTg ctx _ = error "unimplemented" 


startIter :: Context -> IO ()
startIter ctx = foldFree (iterScenarioTg ctx) lobby

newContext :: Token -> Update -> IO Context
newContext token update = Context 
    <$> newChan <*> pure token 
    <*> newChan <*> newChan <*> pure (fromJust $ chatU update)

dispatchUpdate :: Token -> ChatData -> Update -> IO ChatData
dispatchUpdate token cdata update = do
    let chat = fromJust $ chatU update in case chat `M.lookup` cdata of 
        Just ctx -> do
            writeChan (mailbox ctx) update 
            pure $ cdata
        Nothing -> do
            ctx <- newContext token update
            forkIO $ startIter ctx
            pure $ (chat `M.insert` ctx) cdata


dispatchUpdate_ :: Token -> MVar ChatData -> Update -> IO ()
dispatchUpdate_ token var update = do
    cdata <- takeMVar var
    res <- dispatchUpdate token cdata update
    putMVar var res 

safeDispatchUpdate :: Token -> MVar ChatData -> Update -> IO ()
safeDispatchUpdate token var update = do
    cdata <- takeMVar var
    print cdata

    let chat = fromJust $ chatU update 
    
    res <- case chat `M.lookup` cdata of 
            Just ctx -> do
                writeChan (mailbox ctx) update 
                pure $ cdata
            Nothing -> do
                ctx <- newContext token update
                
                let computation = catchAny (startIter ctx) \err -> do 
                    --on high load it can lead to data race so it need to bee rewritten to work atomically 
                        cdata <- takeMVar var        
                        answerWith ctx (TextNButtons ("something went wrong\n\n" ++  show err) (Just [["restart"]]))
                        putMVar var (chat `M.delete` cdata)

                forkIO computation

                pure $ (chat `M.insert` ctx) cdata
    putMVar var res

setupChatData :: IO (MVar ChatData)
setupChatData = do
    newMVar []
