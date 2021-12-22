module Control.Async where 
import Control.Concurrent
import Control.Concurrent.STM (TChan, TVar, readTChan, atomically, newTChan, newTChanIO, writeTChan)
import Control.Concurrent.STM.TSem

{-
throttling mechanism blueprint
-}

type Task = () -> IO ()
type Executor = TChan Task -> IO ()
type Future a = TChan a


excPauseS :: TSem -> Int -> TChan Task -> IO b
excPauseS sem sec chan = do 
    func <- atomically $ do
        waitTSem sem
        readTChan chan
    func ()
    threadDelay $ sec * 1000 * 1000 -- newTSem 1
    atomically $ signalTSem sem
    excPauseS sem sec chan

executeAllWithPause :: Int -> TChan Task -> IO a
executeAllWithPause sec chan = do 
    sem <- atomically $ newTSem 1
    excPauseS sem sec chan

initTasks :: Executor -> IO (TChan Task)
initTasks execute = do
    chan <- newTChanIO :: IO (TChan Task)
    forkIO $ execute chan
    pure chan
    
 
wrapIOToFuture :: TChan Task -> IO a -> IO (Future a)
wrapIOToFuture chan action = do
    var <- newTChanIO  
    atomically $ writeTChan chan $ \_ -> do
        res <- action
        atomically $ writeTChan var res
    pure var

    
awaitAndThen :: Future t -> (t -> IO r) -> IO r
awaitAndThen future action = do
    res <- atomically $ readTChan future
    action res


example = do
    chans <- initTasks $ executeAllWithPause 1

    wrapIOToFuture chans $ putStrLn "d"
    line <- wrapIOToFuture chans getLine 
    wrapIOToFuture chans $ putStrLn "i"
    wrapIOToFuture chans $ putStrLn "c"

    awaitAndThen line print
    wrapIOToFuture chans $ putStrLn "k"

    pure ()