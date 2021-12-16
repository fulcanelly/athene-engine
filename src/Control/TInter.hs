module Control.TInter where


import Control.Concurrent ( Chan, readChan )
import API.Telegram ( Update )
import Control.Async ( Task )
import Control.FreeState ( Scenario, ScenarioF (Eval, Expect, Request) )


newtype OnlyReadChan a = OnlyReadChan { chan :: Chan a }

readORChan :: OnlyReadChan a -> IO a
readORChan = readChan . chan

data Context
    = Context {
        mailbox :: OnlyReadChan Update
        , token :: String
        , syncTasks :: Chan Task
    } 


iterScenarioTg :: Context -> ScenarioF a -> IO a
iterScenarioTg ctx (Eval cmd next) = error "." 
iterScenarioTg ctx (Expect nextF) = error "." 
iterScenarioTg ctx (Request nextF) = error "." 