{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.FreeState where

import Control.Monad.Free ( foldFree, liftF, Free )
import System.Exit ()
import qualified Data.Map as M
import Control.Monad ( guard )
import API.Telegram ( Message (text, Message, message_id, date, from, photo), Update (message, Update, update_id), From (From) )
import Data.Maybe ( fromJust )
import Data.Posts ( AdvPost )
import Control.Exception (throw, catch, Exception, finally)
import Foreign.C (eROFS)
import Data.Data

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

data ReturnE = ReturnE
   deriving (Show)

instance Show ReturnE => Exception ReturnE where

catchReturn :: IO a -> (ReturnE -> IO a) -> IO a
catchReturn = catch

execScenarioTest :: ContextC a -> ScenarioF b -> IO b
execScenarioTest ctx (Expect nextF) = do
    line <- getLine
    let update = updateFromText line
    case ctx of 
        CtxC p -> if p update then throw ReturnE else handleUpdate update
        N -> handleUpdate update
    
    where
    handleUpdate update = case nextF update of
        Just next -> pure next
        Nothing -> execScenarioTest ctx $ Expect nextF

execScenarioTest ctx (Eval cmd next) = do
    case cmd of
        SendWith msg -> putStrLn $ "replying with: " <> mText msg
        _ -> mempty
    pure next

execScenarioTest ctx (ReturnIf pred branch falling) = 
    (execScenarioTest (CtxC pred) `foldFree` branch) `catchReturn` \x -> do
        execScenarioTest ctx `foldFree` falling

execScenarioTest _ _ = do
    error "not implemented"


data ContextC a = CtxC {
    isReturn :: Update -> Bool
    } | N

runScenTest = foldFree $ execScenarioTest N

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


