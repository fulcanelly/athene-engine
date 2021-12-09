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

module Data.Posts where
import Database.SQLite.Simple ( execute, query, field, Only(Only), FromRow(..), Connection )
import Control.Applicative ()
import Database.SQLite.Simple.FromRow ( RowParser )
import Data.Functor.Identity ( Identity )
import Database.SQLite.Simple.FromField ()
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)

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

deriving stock instance Show PartialPost
deriving stock instance Generic PartialPost

deriving anyclass instance ToJSON PartialPost
deriving anyclass instance FromJSON PartialPost

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

updatePost :: Integer -> PartialPost -> Connection -> IO ()
updatePost origin update conn = do
    (Just post) <- getSpecificAt origin conn
    let updater = execute conn "UPDATE channel_posts SET title = ?, channelId = ?, fileId = ?, link = ? WHERE channelId = ?" in
        updater $ updateEntry (summed post) origin
    where 
        updateEntry Post{..} origin = (title, channelId, fileId, link, origin)
        summed post = sumP post update

createNewPost :: AdvPost -> Connection -> IO ()
createNewPost Post{..} conn =
    execute conn "INSERT INTO channel_posts VALUES(?, ?, ?, ?)" postEntry where
        postEntry = (title, channelId, fileId, link)

getSpecificAt :: Integer -> Connection -> IO (Maybe AdvPost)
getSpecificAt channelId conn =
    listToMaybe <$> query conn "SELECT * from channel_posts WHERE channel_id = ?" (Only (channelId :: Integer))

getFewExcept :: Int -> String -> Connection -> IO [AdvPost]
getFewExcept count channelId conn =
    query conn "SELECT * from channel_posts WHERE channel_id != ? LIMIT ?" (channelId, count)

    