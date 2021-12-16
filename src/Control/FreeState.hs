{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Control.FreeState where

import Control.Monad.Free
import System.Exit hiding (ExitSuccess)
import qualified Data.Map as M
import Control.Monad
import API.Telegram ( Message (text, Message, message_id, date, from, photo), Update (message, Update, update_id), From (From) )
import Data.Maybe
import Data.Posts

data MessageEntry
    = Text {
        mText:: String
    }
    | NoN

replyText :: String -> Command
replyText text = ReplyWith $ Text text



data Command
    = None
    | ReplyWith MessageEntry
    | AnswerWith MessageEntry
    | CreatePost AdvPost



data ScenarioF next
    = Expect (Update -> Maybe next)
    | Eval Command next
    | JumpBackIf next
    deriving Functor


type Scenario = Free ScenarioF


eval :: Command -> Scenario ()
eval cmd = liftF $ Eval cmd ()

expect :: (Update -> Maybe a) -> Scenario a
expect pred = liftF $ Expect pred




data Context
    = Context {

    }



execScenarioTest :: p -> ScenarioF a -> IO a
execScenarioTest ctx (Expect nextF) = do
    line <- getLine
    case nextF $ updateFromText line of
        Just next -> pure next
        Nothing -> execScenarioTest ctx $ Expect nextF

execScenarioTest ctx (Eval cmd next) = do
    case cmd of
        ReplyWith msg -> putStrLn $ "replying with: " <> mText msg
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
        from = undefined,
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
    eval (replyText $ "you said " <> text <> ", you can *repeat* again")
    text <- expect anyText
    if text == "repeat"
        then start
        else

            eval $ replyText "we are done"
    eval $ replyText "hahaha"

    eval None


