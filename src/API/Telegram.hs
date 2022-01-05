

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

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
      FromJSON (),
      Array,
      Object,
      Value,
      ToJSON )

import Data.List ( intercalate )
import GHC.Generics ( Generic )
import Data.Either.Combinators ( rightToMaybe )
import Data.Aeson.Types ( parseEither, Result (Success) )
import qualified Data.Vector as V
import Data.Maybe ( fromJust )
import Data.Functor ()
import Control.Monad ()
import API.ReplyMarkup ( KeyboardButton, kbToJSON )
import Data.Generics.Labels ()
import Control.Lens ( (^?), (^.), _Just )
import Network.Wreq.Session
import Network.HTTP.Simple (getResponseBody)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (newTChan, newTChanIO)
import Control.Async

data PhotoEntry = PhotoEntry {
    file_id :: String
    , width :: Int
    , height :: Int
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)

data From = From {
        id :: Int
        , first_name :: String
        , username :: Maybe String
        , is_bot :: Bool
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)

data Message = Message {
        message_id :: Int
        , date :: Int
        , from :: From
        , text :: Maybe String
        , photo :: Maybe [PhotoEntry]
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)

data Update =
    Update {
        update_id :: Integer
        , message :: Maybe Message
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)


fView = flip (^.)
fSafeView = flip (^?)

msgIdU :: Update -> Maybe Int
msgIdU = fSafeView $ #message . _Just . #message_id

chatU :: Update -> Maybe Int
chatU = fSafeView $ #message . _Just . #from . #id

textU :: Update -> Maybe String
textU = fView $ #message . _Just . #text


openHTTPS :: String -> IO LB.ByteString
openHTTPS = HC.simpleHttp


data Request =
    GetMe |
    GetUpdates |
    SendMessage |
    SendPhoto

instance Show Request where
    show GetMe = "getMe"
    show GetUpdates = "getUpdates"
    show SendMessage = "sendMessage"
    show SendPhoto = "sendPhoto"


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


type ReqExecutor = Request -> M.Map String String -> IO LB.ByteString

execArgsTg :: Session -> String -> ReqExecutor
execArgsTg session token req args = do
    getResponseBody <$> get session url
    where url = "https://api.telegram.org/bot" ++ token ++ "/" ++ show req ++ "?" ++ mapToArgs args

obtainExec :: String -> IO ReqExecutor
obtainExec token = execArgsTg <$> newSession <*> pure token



execArgsTgJson :: ReqExecutor -> Request -> M.Map String String -> IO (Maybe Object)
execArgsTgJson exec req args = do
        res <- exec req args
        pure $ J.decode res

getKey :: FromJSON b => Object -> Key -> Maybe b
getKey key = rightToMaybe . parseEither (key .:)

getUpdatesJson :: ReqExecutor -> M.Map String String -> IO (V.Vector Update)
getUpdatesJson exec args = do
    obj <- execArgsTgJson exec GetUpdates args
    let (Just arr) = (fromJust obj `getKey` "result" :: Maybe Array)
    pure $ V.map (\x -> do
            let (Success update) = fromJSON x
            update
        ) arr


forAllUpdates :: ReqExecutor -> (Update -> IO ()) -> Maybe Integer -> IO ()
forAllUpdates exec handler updateId = do
    updates <- getUpdates' updateId
    V.mapM_ handler updates

    forAllUpdates exec handler $ lastU' updates
    where
        lastU = Just . (+) 1 . update_id . V.head
        lastU' v = if V.null v then
            updateId else lastU v
        getUpdates' Nothing = getUpdatesJson exec M.empty
        getUpdates' (Just updateId) = getUpdatesJson exec $ M.fromList [("offset", show updateId)]


unjust :: (a -> Maybe c) -> a -> c
unjust x = fromJust . x

sendMessageWithArgs :: ReqExecutor -> ChatId -> String -> Args -> IO Message
sendMessageWithArgs exec chat text args = do
    res <- execArgsTgJson exec SendMessage params
    let (Just obj) = res >>= (`getKey` "result") :: Maybe Value
    let (Success msg) = fromJSON obj
    pure msg
    where
        params = [("text", text), ("chat_id", show chat)] `M.union` args


sendGenericMessageWithArgs :: ReqExecutor -> ChatId -> Request -> Args -> IO Message
sendGenericMessageWithArgs exec chat req args  = do
    res <- execArgsTgJson exec req params
    let (Just obj) = res >>= (`getKey` "result") :: Maybe Value
    let (Success msg) = fromJSON obj
    pure msg
    where
        params = [("chat_id", show chat)] `M.union` args



type Args = M.Map String String
type Token = String
type ChatId = Int
type MsgId = Int
type FileId = String

answerWithButtons :: ReqExecutor -> ChatId -> String -> [[KeyboardButton]] -> IO Message
answerWithButtons exec chat text btns = do
    sendMessageWithArgs exec chat text [("reply_markup", kbToJSON btns)]

answer :: ReqExecutor -> ChatId -> String -> IO Message
answer exec chat text = sendMessageWithArgs exec chat text M.empty

reply :: ReqExecutor -> ChatId -> MsgId -> String -> IO Message
reply exec chat msgId text = do
    sendMessageWithArgs exec chat text [("reply_to_message_id", show msgId)]

replyWithButtons token chat msgId text btns = do
    sendMessageWithArgs token chat text [
        ("reply_to_message_id", show msgId),
        ("reply_markup", kbToJSON btns)
        ]
    