{-# LANGUAGE LambdaCase #-}
module Data.Favorites where
import Database.SQLite.Simple 
import API.Telegram 
import Data.Maybe

data Favorite 
    = Favorite {
        userId :: ChatId
        , subject :: ChatId
        , isLiked :: Bool
    }

instance FromRow Favorite where
    fromRow = Favorite <$> field <*> field <*> field 

setupDB :: Connection -> IO ()
setupDB conn = execute conn "CREATE TABLE IF NOT EXISTS channel_favorites(is_liked, subject, user_id)" ()

likePostBy :: ChatId -> ChatId -> Connection -> IO ()
likePostBy subject userId conn = do
    (subject `createOrUpdateIfPresent` userId) True conn

dislikePostBy :: ChatId -> ChatId -> Connection -> IO ()
dislikePostBy subject userId conn = do
    (subject `createOrUpdateIfPresent` userId) False conn

createOrUpdateIfPresent :: ChatId -> ChatId -> Bool -> Connection -> IO ()
createOrUpdateIfPresent subject userId state conn = do
    res <- (subject `getRateBy` userId) conn 
    (case res of 
        Nothing -> createLikeEntry
        Just fav -> updateLikeEntry
        ) subject userId state conn  

updateLikeEntry :: ChatId -> ChatId -> Bool -> Connection -> IO ()
updateLikeEntry subject userId state conn = execute conn "UPDATE channel_favorites SET is_liked = ? WHERE subject = ? AND user_id = ?" (state, subject, userId)

createLikeEntry :: ChatId -> ChatId -> Bool -> Connection -> IO ()
createLikeEntry subject userId state conn = (conn `execute` "INSERT INTO channel_favorites VALUES (?, ?, ?)") (state, subject, userId)

getRateBy :: ChatId -> ChatId -> Connection -> IO (Maybe Favorite)
getRateBy subject userId conn = 
    listToMaybe <$> query conn "SELECT * FROM channel_favorites WHERE user_id = ?" (Only (userId :: Int))