{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.Telegram where

import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as M
import qualified Network.URI.Encode as N ( encode )
import Data.Aeson as J
    ( decode,
      (.:),
      fromJSON,
      Key,
      FromJSON,
      Array,
      Object,
      Result,
      Value,
      ToJSON )

import qualified Data.String as S
import Data.List ( intercalate )
import GHC.Generics ( Generic )
import Data.Either.Combinators ( rightToMaybe )
import Data.Aeson.Types ( parseEither, Parser, Result (Success) )
import qualified Data.Vector as V

data PhotoEntry = PhotoEntry {
    file_id :: String
    , width :: Int
    , height :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data From = From {
        id :: Int
        , first_name :: String
        , username :: Maybe String
        , is_bot :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Message = Message {
        message_id :: Int
        , date :: Int
        , from :: From
        , text :: Maybe String
        , photo :: Maybe [PhotoEntry]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Update =
    Update {
        update_id :: Integer
        , message :: Maybe Message
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

openHTTPS :: String -> IO LB.ByteString
openHTTPS = HC.simpleHttp


token :: [Char]
token = "2094069209:AAHoBnp3rbASgqR4ZNzgN26MJZWT8jW9xX4"


data Request =
    GetMe |
    GetUpdates

instance Show Request where
    show GetMe = "getMe"
    show GetUpdates = "getUpdates"

execTgGeneric :: [Char] -> [Char] -> IO LB.ByteString
execTgGeneric token x = openHTTPS $ "https://api.telegram.org/bot" ++ token ++ "/" ++ x

execSimpleTg :: Show a => [Char] -> a -> IO LB.ByteString
execSimpleTg token req = execTgGeneric token (show req)

pack :: [Char] -> LB.ByteString
pack = LB.pack

unpack :: LB.ByteString -> [Char]
unpack = LB.unpack

mapToArgs :: M.Map String String -> String
mapToArgs = intercalate "&"
    . zip'
    . unzip
    . M.toList
    where zip' (x, y) = zipWith (\a b -> a ++ "=" ++ N.encode b) x y


execArgsTg :: Show a => String -> a -> M.Map String String -> IO LB.ByteString
execArgsTg token req args = do
    openHTTPS url
    where url = "https://api.telegram.org/bot" ++ token ++ "/" ++ show req ++ "?" ++ mapToArgs args


getKey :: FromJSON b => Object -> Key -> Maybe b
getKey key = rightToMaybe . parseEither (key .:)

getUpdatesJson :: String -> M.Map String String -> IO (V.Vector Update)
getUpdatesJson token args = do
    res <- execArgsTg token GetUpdates args
    let (Just obj) = J.decode res :: Maybe Object
    let (Just arr) = (obj `getKey` "result" :: Maybe Array)
    pure $ V.map (\x -> do
            let (Success update) = fromJSON x :: Result Update
            update
        ) arr

forAllUpdates :: String -> (Update -> IO ()) -> Maybe Integer -> IO ()
forAllUpdates token handler updateId = do
    updates <- getUpdates' updateId
    V.mapM_ handler updates

    forAllUpdates token handler $ lastU' updates
    where
        lastU = Just . (+) 1 . update_id . V.head
        lastU' v = if V.null v then
            updateId else lastU v
        getUpdates' Nothing = getUpdatesJson token M.empty
        getUpdates' (Just updateId) = getUpdatesJson token $ M.fromList [("offset", show updateId)]
