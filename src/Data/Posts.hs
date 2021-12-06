{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Posts where
import Database.SQLite.Simple ( execute, query, field, Only(Only), FromRow(..), Connection )
import Control.Applicative ()
import Database.SQLite.Simple.FromRow ( RowParser )
import Data.Functor.Identity ( Identity )
import Database.SQLite.Simple.FromField ()
import Data.Maybe ( fromMaybe, listToMaybe )

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
type PartialPost = AdvPostTemplate Maybe

instance FromRow AdvPost where
    fromRow = Post <$> field <*> field <*> field <*> field

sumP :: AdvPost -> PartialPost -> AdvPost
sumP post pPost = post {
        title = unpackM title (title post),
        channelId = unpackM channelId (channelId post),
        fileId = unpackM fileId (fileId post),
        link = unpackM link (link post)
    }
    where unpackM field = flip fromMaybe $ field pPost
-- can't make work: unpackM field = unpackM (field pPost) (field post)

setupDB :: Connection -> IO ()
setupDB conn =
    execute conn query ()
    where query = "CREATE TABLE IF NOT EXISTS channel_posts(\
        \ title, channel_id, file_id, link, \
        \ FOREIGN KEY(channel_id) REFERENCES channel(id))"

updatePost :: AdvPost -> PartialPost -> Connection -> IO ()
updatePost post update conn = 
    let origin = channelId post in 
    let updater = execute conn "UPDATE channel_posts SET title = ?, channelId = ?, fileId = ?, link = ? WHERE channelId = ?" in
        updater updateEntry summed origin
    where 
        updateEntry Post{..} origin = (title, channelId, fileId, link, origin)
        summed = sumP post update

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

    