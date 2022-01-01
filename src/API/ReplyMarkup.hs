{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}


module API.ReplyMarkup where
import GHC.Generics
import Data.Aeson
import Data.Text.Encoding
import Data.ByteString.Lazy (toStrict)
import Data.Text

newtype KeyboardButton = KButton {
        text :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ReplyKeyboardMarkup =
    ReplyKeyboardMarkup {
        keyboard :: Maybe [[KeyboardButton]],
        resize_keyboard :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ReplyKeyboardRemove =
    ReplyKeyboardRemove {
        remove_keyboard :: Bool,
        selective :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


data InlineKeyboardButton = 
    InlineKeyboardButton {
        text_ :: String, --- problem !!!!
        url :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype InlineKeyboardMarkup =
    InlineKeyboardMarkup {
        inline_keyboard :: [[InlineKeyboardButton]]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

textButton = KButton 

markupOfBtn mark = ReplyKeyboardMarkup (Just mark) False

kbToJSON :: [[KeyboardButton]] -> String
kbToJSON = toJSONString . markupOfBtn


toJSONString :: ToJSON a => a -> String
toJSONString = unpack . decodeUtf8 . toStrict . encode

disableKb = ReplyKeyboardRemove True Nothing 