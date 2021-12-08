module Control.Async where 
import Control.Concurrent

{-
throttling mechanism blueprint
-}

type Task = () -> IO ()
type Executor = Chan Task -> IO ()
type Future a = MVar a

executeAllWithPause :: Int -> Executor
executeAllWithPause sec chan = do
    func <- readChan chan
    func ()
    threadDelay $ sec * 1000 * 1000
    executeAllWithPause sec chan

initTasks :: Executor -> IO (Chan Task)
initTasks execute = do
    chan <- newChan :: IO (Chan Task)
    forkIO $ execute chan
    pure chan
    
 
wrapIOToFuture :: Chan Task -> IO a -> IO (Future a)
wrapIOToFuture chan action = do
    var <- newEmptyMVar 
    writeChan chan $ \() -> do
        res <- action
        putMVar var res
    pure var

    
awaitAndThen :: Future t -> (t -> IO r) -> IO r
awaitAndThen future action = do
    res <- takeMVar future
    action res

