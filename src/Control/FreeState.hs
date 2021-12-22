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
import API.Telegram 
import Data.Maybe ( fromJust )
import Data.Posts ( AdvPost, AdvPostTemplate (Post) )
import Control.Exception (throw, catch, Exception, finally)
import Data.Data
import Data.Functor.Compose (Compose(Compose, getCompose))
import API.ReplyMarkup
import Data.ByteString.Builder.Prim (primMapByteStringBounded)

data MessageEntry
    = MessageEntry { 
        args :: Args
        , method :: Request
    }
    deriving Show

sendText :: String -> Command
sendText text = SendWith $ MessageEntry [("text", text)] SendMessage 

strTreeToButtons :: [[String]] -> String
strTreeToButtons buttons = kbToJSON . getCompose $ KButton <$> Compose buttons

sendPhoto :: FileId -> String -> [[String]] -> MessageEntry
sendPhoto fileId caption buttons = MessageEntry {
        args = [
            ("reply_markup", strTreeToButtons buttons),
            ("caption", caption),
            ("photo", fileId)
        ],
        method = SendPhoto
    } 
    
sendTextNButtonsEntry :: String -> [[String]] -> MessageEntry
sendTextNButtonsEntry text buttons = MessageEntry [
    ("text", text), ("reply_markup", strTreeToButtons buttons)] SendMessage


data Command
    = None
    | SendWith MessageEntry
    | CreatePost AdvPost
    | LikePost AdvPost
    | DislikePost AdvPost

data DBRequest

data ScenarioF next
    = Expect (Update -> Maybe next)
    | Eval Command next
    | ReturnIf (Update -> Bool) (Scenario next) (Scenario next)
    | FindRandPost (Maybe AdvPost -> next)
    | LoadMyPost (Maybe AdvPost -> next)

deriving instance Functor ScenarioF

type Scenario = Free ScenarioF


eval :: Command -> Scenario ()
eval cmd = liftF $ Eval cmd ()

loadMyPost :: Scenario (Maybe AdvPost)
loadMyPost = liftF $ LoadMyPost Prelude.id

findRandPost :: Scenario (Maybe AdvPost)
findRandPost = liftF $ FindRandPost Prelude.id

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
        SendWith msg -> print msg
        _ -> mempty
    pure next

execScenarioTest ctx (ReturnIf pred branch falling) =
    (execScenarioTest (CtxC pred) `foldFree` branch) `catchReturn` const handleFalling
    where handleFalling = execScenarioTest ctx `foldFree` falling


execScenarioTest _ _ = error "unimplemented"

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
