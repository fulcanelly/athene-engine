{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.Posts where

import Database.SQLite.Simple hiding (execute, query)
import Control.Applicative ()
import Database.SQLite.Simple.FromRow ( RowParser )
import Data.Functor.Identity ( Identity )
import Database.SQLite.Simple.FromField ()
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import Control.Applicative
import API.Telegram (ChatId)
import Control.Database hiding (ChatId)
import Control.Lens (makeLenses)


data AdvPost = Post {
    _title :: String
    , _userId :: Int
    , _fileId :: String -- adv photo 
    , _link :: String
    }

$(makeLenses ''AdvPost)

deriving stock instance Generic AdvPost

instance FromRow AdvPost where
    fromRow = Post <$> field <*> field <*> field <*> field

setupDB :: SqlRequest ()
setupDB =
    execute query ()
    where query = "CREATE TABLE IF NOT EXISTS channel_posts(\
        \ title, user_id, file_id, link)"

updatePost :: AdvPost -> SqlRequest Bool
updatePost Post{..} = do
    jpost <- getSpecificAt _userId  
    case jpost of 
        Nothing -> pure False 
        Just post -> do
            execute "UPDATE channel_posts SET title = ?, file_id = ?, link = ? WHERE user_id = ?" updateEntry
            pure True
    where
    updateEntry = (_title, _fileId, _link, _userId)

createNewPost :: AdvPost -> SqlRequest ()
createNewPost Post{..}  =
    execute "INSERT INTO channel_posts VALUES(?, ?, ?, ?)" postEntry where
        postEntry = (_title, _userId, _fileId, _link)

getSpecificAt :: Int -> SqlRequest (Maybe AdvPost)
getSpecificAt userId =
    listToMaybe <$> query "SELECT * from channel_posts WHERE user_id = ?" (Only (userId :: Int))

getFewExcept :: Int -> String -> SqlRequest [AdvPost]
getFewExcept count userId =
    query "SELECT * from channel_posts WHERE user_id != ? LIMIT ?" (userId, count)

findRandomPostExcluding :: ChatId  -> SqlRequest (Maybe AdvPost)
findRandomPostExcluding userId  =
    listToMaybe <$> query  "SELECT * FROM channel_posts WHERE user_id != ? ORDER BY RANDOM() LIMIT 1" (Only (userId :: Int))