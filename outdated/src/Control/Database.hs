{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Database where
import Database.SQLite.Simple hiding (execute, execute_, query)
import qualified Database.SQLite.Simple as S (execute, query)

import Control.Monad
import Control.Monad.Free
import Data.Maybe
import Control.Concurrent.STM
import Control.Async

data SQLnTasks
    = SQLnTasks {
        conn :: Connection,
        tasks :: TChan Task
    }

runTransaction :: SQLnTasks -> SqlRequest a -> IO (Future a)
runTransaction SQLnTasks{..} transaction = 
    tasks `runAsync` do 
        conn `runSql` transaction 
        
data SqlRequestF next
    = forall t f. (ToRow t, FromRow f) => QueryF Query t ([f] -> next)
    | forall t. (ToRow t) => Execute Query t next

deriving instance Functor SqlRequestF 

type SqlRequest = Free SqlRequestF


query :: (ToRow t, FromRow a) => Query -> t -> SqlRequest [a]
query q a = liftF $ QueryF q a id

execute :: ToRow t => Query -> t -> SqlRequest ()
execute q a = liftF $ Execute q a ()

execute_ :: Query -> SqlRequest ()
execute_ = flip execute ()

-- natural transformation 

silentRun :: Connection -> SqlRequestF next -> IO next
silentRun conn (Execute query row next) = do
    (conn `S.execute` query) row
    pure next

silentRun conn (QueryF query row next) = 
    next <$> S.query conn query row

-- interpreter

runSql :: Connection -> SqlRequest a -> IO a
runSql conn x = do 
    foldFree (silentRun conn) x  



