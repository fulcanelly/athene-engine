{-# LANGUAGE LambdaCase #-}
module Data.Favorites where
import Database.SQLite.Simple hiding (execute, query, execute_, query_)
import API.Telegram
import Data.Maybe
import Control.Database

data Favorite
    = Favorite {
        userId :: ChatId
        , subject :: ChatId
        , isLiked :: Bool
    } deriving Show 

instance FromRow Favorite where
    fromRow = Favorite <$> field <*> field <*> field

setupDB :: SqlRequest ()
setupDB = execute_  "CREATE TABLE IF NOT EXISTS channel_favorites(is_liked, subject, user_id, \
                \ FOREIGN KEY (subject) REFERENCES channel_posts(user_id), \
                \ FOREIGN KEY (user_id) REFERENCES channel_posts(user_id) \
                \ )"

likePostBy :: ChatId -> ChatId -> SqlRequest ()
likePostBy subject userId = do
    (subject `createOrUpdateIfPresent` userId) True

dislikePostBy :: ChatId -> ChatId -> SqlRequest ()
dislikePostBy subject userId = do
    (subject `createOrUpdateIfPresent` userId) False

createOrUpdateIfPresent :: ChatId -> ChatId -> Bool -> SqlRequest ()
createOrUpdateIfPresent subject userId state = do
    res <- subject `getRateBy` userId
    (case res of
        Nothing -> createLikeEntry
        Just fav -> updateLikeEntry
        ) subject userId state

updateLikeEntry :: ChatId -> ChatId -> Bool -> SqlRequest ()
updateLikeEntry subject userId state = execute "UPDATE channel_favorites SET is_liked = ? WHERE subject = ? AND user_id = ?" (state, subject, userId)

createLikeEntry :: ChatId -> ChatId -> Bool -> SqlRequest ()
createLikeEntry subject userId state = execute "INSERT INTO channel_favorites VALUES (?, ?, ?)" (state, subject, userId)

getRateBy :: ChatId -> ChatId -> SqlRequest (Maybe Favorite)
getRateBy subject userId =
    listToMaybe <$> query "SELECT * FROM channel_favorites WHERE subject = ? AND user_id = ?" (subject, userId)