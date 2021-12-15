{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}


module API.Keyboard where
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB

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

textButton = KButton 

markupOfBtn mark = ReplyKeyboardMarkup (Just mark) False

kbToString :: [[KeyboardButton]] -> String
kbToString = LB.unpack . encode . markupOfBtn