{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}


module API.ReplyMarkup where
import GHC.Generics
import Data.Aeson
import Data.Text.Encoding
import Data.ByteString.Lazy (toStrict)
import Data.Text
import Deriving.Aeson

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
    IKB {
        text_ :: String,
        url :: String
    } deriving stock (Show, Generic)
    deriving (ToJSON, FromJSON) via (CustomJSON '[
        FieldLabelModifier (Rename "text_" "text")]
        InlineKeyboardButton)


newtype InlineKeyboardMarkup =
    InlineKeyboardMarkup {
        inline_keyboard :: [[InlineKeyboardButton]]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

textButton = KButton

markupOfBtn :: [[KeyboardButton]] -> ReplyKeyboardMarkup
markupOfBtn mark = ReplyKeyboardMarkup (Just mark) False

imarkupOfBtn :: [[InlineKeyboardButton]] -> InlineKeyboardMarkup
imarkupOfBtn = InlineKeyboardMarkup

kbToJSON :: [[KeyboardButton]] -> String
kbToJSON = toJSONString . markupOfBtn


ikbToJSON :: [[InlineKeyboardButton]] -> String
ikbToJSON = toJSONString . imarkupOfBtn



toJSONString :: ToJSON a => a -> String
toJSONString = unpack . decodeUtf8 . toStrict . encode

disableKb = ReplyKeyboardRemove True Nothing