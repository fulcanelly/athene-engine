{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Control.FreeState where

import Control.Monad.Free ( foldFree, liftF, Free )
import System.Exit ()
import qualified Data.Map as M
import Control.Monad ( guard )
import API.Telegram ( Message (text, Message, message_id, date, from, photo), Update (message, Update, update_id), From (From) )
import Data.Maybe ( fromJust )
import Data.Posts ( AdvPost )

data MessageEntry
    = Text {
        mText :: String
    }
    | TextNButtons {
        mText :: String,
        buttons :: [[String]] 
    }
    | ReplyText {
        mText :: String
    }
    | ReplyTextNButtons {
        mText :: String,
        buttons :: [[String]] 
    } 

sendText :: String -> Command
sendText text = SendWith $ Text text


data Command
    = None
    | SendWith MessageEntry
    | CreatePost AdvPost


data DBRequest
    
data ScenarioF next
    = Expect (Update -> Maybe next)
    | Eval Command next
    | ReturnIf (Update -> Bool) (Scenario next) (Scenario next)

deriving instance Functor ScenarioF

type Scenario = Free ScenarioF


eval :: Command -> Scenario ()
eval cmd = liftF $ Eval cmd ()

expect :: (Update -> Maybe a) -> Scenario a
expect pred = liftF $ Expect pred

returnIf :: (Update -> Bool) -> Scenario a -> Scenario a -> Scenario a
returnIf pred branch failbranch = liftF $ ReturnIf pred branch failbranch

execScenarioTest :: p -> ScenarioF a -> IO a
execScenarioTest ctx (Expect nextF) = do
    line <- getLine
    case nextF $ updateFromText line of
        Just next -> pure next
        Nothing -> execScenarioTest ctx $ Expect nextF

execScenarioTest ctx (Eval cmd next) = do
    case cmd of
        SendWith msg -> putStrLn $ "replying with: " <> mText msg
        _ -> mempty
    pure next

execScenarioTest ctx _ = do
    pure undefined

test = foldFree $ execScenarioTest undefined

updateFromText text = Update {
    update_id = 1,
    message = Just Message {
        message_id = 1,
        date = 1,
        from = error "never used",
        text = Just text,
        photo = Nothing
    }
}



anyText :: Update -> Maybe String
anyText = text . fromJust . message

a = guard

expectText :: String -> Scenario String
expectText expected = do
    text <- expect anyText
    if text == expected
        then pure text
        else expectText expected


start :: Scenario ()
start = do

    text <- expectText "loh"
    eval (sendText $ "you said " <> text <> ", you can *repeat* again")
    text <- expect anyText
    if text == "repeat"
        then start
        else
            eval $ sendText "we are done"
    eval $ sendText "hahaha"

    eval None


