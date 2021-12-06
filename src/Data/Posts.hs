{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Posts where
import Database.SQLite.Simple
import Control.Applicative
import Database.SQLite.Simple.FromRow ( RowParser )
import Data.Maybe (listToMaybe)
import Data.Functor.Identity
import Database.SQLite.Simple.FromField

data AdvPostTemplate f = Post {
    title :: f String
    , channelId :: f Integer
    , fileId :: f Integer -- adv photo 
    , link :: f String
}

type AdvPost = AdvPostTemplate Identity


ifield :: FromField a => RowParser (Identity a)
ifield = Identity <$> field

instance FromRow AdvPost where
    fromRow = Post <$> ifield <*> ifield <*> ifield <*> ifield


setupDB :: Connection -> IO ()
setupDB conn =
    execute conn query ()
    where query = "CREATE TABLE IF NOT EXISTS channel_posts(\
        \ title, channel_id, file_id, link, \
        \ FOREIGN KEY(channel_id) REFERENCES channel(id))"

createNewPost :: AdvPost -> Connection -> IO ()
createNewPost Post{..} conn  =
    execute conn "INSERT INTO channel_posts VALUES(?, ?, ?, ?)" postEntry where
        postEntry = (runIdentity title, runIdentity channelId, runIdentity fileId, runIdentity link)

getSpecificAt :: String -> Connection -> IO (Maybe AdvPost)
getSpecificAt channelId conn=
    listToMaybe <$> query conn "SELECT * from channel_posts WHERE channel_id = ?" (Only (channelId :: String))

getFewExcept :: Int -> String -> Connection -> IO [AdvPost]
getFewExcept count channelId conn =
    query conn "SELECT * from channel_posts WHERE channel_id != ? LIMIT ?" (channelId, count)

    