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

type family AnyOrId s a where
    AnyOrId Identity a = a
    AnyOrId s a = s a

data AdvPostTemplate f = Post {
    title :: AnyOrId f String
    , userId :: AnyOrId f Int
    , fileId :: AnyOrId f String -- adv photo 
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

instance Semigroup (AdvPostTemplate Maybe) where
    a <> b = Post
        (alt title)
        (alt userId)
        (alt fileId)
        (alt link) where
            alt f = f a <|> f b


instance Monoid PartialPost where
    mempty = emptyP

sumP :: AdvPost -> PartialPost -> AdvPost
sumP post pPost = post {
        title = unpackM title (title post),
        userId = unpackM userId (userId post),
        fileId = unpackM fileId (fileId post),
        link = unpackM link (link post)
    }
    where unpackM field = flip fromMaybe $ field pPost
-- can't make work: unpackM field = unpackM (field pPost) (field post)

emptyP = Post Nothing Nothing Nothing Nothing

setupDB :: SqlRequest ()
setupDB =
    execute query ()
    where query = "CREATE TABLE IF NOT EXISTS channel_posts(\
        \ title, user_id, file_id, link)"

updatePost :: Int -> PartialPost -> SqlRequest Bool
updatePost origin update = do
    jpost <- getSpecificAt origin  
    case jpost of 
        Nothing -> pure False 
        Just post -> do
            execute "UPDATE channel_posts SET title = ?, user_id = ?, file_id = ?, link = ? WHERE user_id = ?" (updateEntry (summed post) origin)
            pure True
    where
    updateEntry Post{..} origin = (title, userId, fileId, link, origin)
    summed post = sumP post update

createNewPost :: AdvPost -> SqlRequest ()
createNewPost Post{..}  =
    execute "INSERT INTO channel_posts VALUES(?, ?, ?, ?)" postEntry where
        postEntry = (title, userId, fileId, link)

getSpecificAt :: Int -> SqlRequest (Maybe AdvPost)
getSpecificAt userId =
    listToMaybe <$> query "SELECT * from channel_posts WHERE user_id = ?" (Only (userId :: Int))

getFewExcept :: Int -> String -> SqlRequest [AdvPost]
getFewExcept count userId =
    query "SELECT * from channel_posts WHERE user_id != ? LIMIT ?" (userId, count)

findRandomPostExcluding :: ChatId  -> SqlRequest (Maybe AdvPost)
findRandomPostExcluding userId  =
    listToMaybe <$> query  "SELECT * FROM channel_posts WHERE user_id != ? ORDER BY RANDOM() LIMIT 1" (Only (userId :: Int))