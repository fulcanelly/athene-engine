module Control.Async where 
import Control.Concurrent
import Control.Concurrent.STM (TChan, TVar, readTChan, atomically, newTChan, newTChanIO, writeTChan)
import Control.Concurrent.STM.TSem
import Control.Monad (forM_, forever)
import Control.Exception

{-
throttling mechanism blueprint
-}

type Task = () -> IO ()
type Executor = TChan Task -> IO ()
type Future a = TChan a


excPauseS :: TSem -> Int -> Executor
excPauseS sem sec chan = do 
    func <- atomically $ do
        waitTSem sem
        readTChan chan
    func ()
    threadDelay $ sec * 1000 * 1000
    atomically $ signalTSem sem
    excPauseS sem sec chan

executeAllWithPause :: Int -> Executor
executeAllWithPause sec chan = do 
    sem <- atomically $ newTSem 1
    excPauseS sem sec chan

executeAsPossible :: Executor
executeAsPossible chan = do
    sem <- atomically $ newTSem 1
    forever $ exec sem chan 
    `catch` (print :: BlockedIndefinitelyOnSTM -> IO ())
    `catch` \e -> do
        print (e :: SomeException)
        putStrLn "got problem in async, restarting"
        executeAsPossible chan

    where 
    exec sem chan = do
        func <- atomically $ do
            waitTSem sem
            readTChan chan
        func ()
        atomically $ signalTSem sem

    
initTasks :: Executor -> IO (TChan Task)
initTasks execute = do
    chan <- newTChanIO :: IO (TChan Task)
    forkIO $ execute chan
    pure chan
    
 
runAsync :: TChan Task -> IO a -> IO (Future a)
runAsync = wrapIOToFuture

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

awaitIOAndThen :: IO (Future t) -> (t -> IO r) -> IO r
awaitIOAndThen future action =
    (`awaitAndThen` action) =<< future

testSem = do 
    sem <- atomically $ newTSem 1
    let jobs = [ sem `runInSem` printPaused i | i <- [1..]]
    take 30 jobs `forM_` forkIO
  

    where 
    runInSem sem io = do
        atomically $ waitTSem sem
        io
        atomically $ signalTSem sem
    printPaused x = do 
        print x
        threadDelay $  100 * 1000
example = do
    chans <- initTasks $ executeAllWithPause 1

    wrapIOToFuture chans $ putStrLn "d"
    line <- wrapIOToFuture chans getLine 
    wrapIOToFuture chans $ putStrLn "i"
    wrapIOToFuture chans $ putStrLn "c"

    awaitAndThen line print
    wrapIOToFuture chans $ putStrLn "k"

    pure ()