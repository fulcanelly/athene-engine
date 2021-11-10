{-# LANGUAGE OverloadedStrings #-}

module Data.Posts where
import Database.SQLite.Simple
import Control.Applicative
import Database.SQLite.Simple.FromRow
import Data.Maybe (listToMaybe)

data AdvPost = Post {
    title :: String
    , channelId :: Integer
    , fileId :: Integer -- adv photo 
    , link :: String
} deriving (Eq, Show)


instance FromRow AdvPost where
    fromRow = Post <$> field <*> field <*> field <*> field


setupDB :: Connection -> IO ()
setupDB conn =
    execute conn "CREATE TABLE IF NOT EXISTS channel_posts(title, channel_id, file_id, link)" ()

getSpecificAt :: String -> Connection -> IO (Maybe AdvPost)
getSpecificAt channelId conn=
    listToMaybe <$> query conn "SELECT * from channel_posts WHERE channel_id = ?" (Only (channelId :: String)) 

getFewExcept :: Int -> String -> Connection -> IO [AdvPost]
getFewExcept count channelId conn =
    query conn "SELECT * from channel_posts WHERE channel_id != ? LIMIT ?" (channelId, count) 