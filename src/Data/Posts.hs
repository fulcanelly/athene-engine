{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Posts where
import Database.SQLite.Simple
import Control.Applicative
import Database.SQLite.Simple.FromRow ( RowParser )
import Data.Maybe (listToMaybe)
import Data.Functor.Identity
import Database.SQLite.Simple.FromField


type family AnyOrId s a where
    AnyOrId Identity a = a
    AnyOrId s a = s a

data AdvPostTemplate f = Post {
    title :: AnyOrId f String
    , channelId :: AnyOrId f Integer
    , fileId :: AnyOrId f Integer -- adv photo 
    , link :: AnyOrId f String
}


type AdvPost = AdvPostTemplate Identity

instance FromRow AdvPost where
    fromRow = Post <$> field <*> field <*> field <*> field


setupDB :: Connection -> IO ()
setupDB conn =
    execute conn query ()
    where query = "CREATE TABLE IF NOT EXISTS channel_posts(\
        \ title, channel_id, file_id, link, \
        \ FOREIGN KEY(channel_id) REFERENCES channel(id))"

createNewPost :: AdvPost -> Connection -> IO ()
createNewPost Post{..} conn  =
    execute conn "INSERT INTO channel_posts VALUES(?, ?, ?, ?)" postEntry where
        postEntry = (title, channelId, fileId, link)

getSpecificAt :: String -> Connection -> IO (Maybe AdvPost)
getSpecificAt channelId conn=
    listToMaybe <$> query conn "SELECT * from channel_posts WHERE channel_id = ?" (Only (channelId :: String))

getFewExcept :: Int -> String -> Connection -> IO [AdvPost]
getFewExcept count channelId conn =
    query conn "SELECT * from channel_posts WHERE channel_id != ? LIMIT ?" (channelId, count)

    