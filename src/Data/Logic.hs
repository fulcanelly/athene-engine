{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Logic where
import Control.FreeState as F

import Data.Posts ( AdvPost, AdvPostTemplate(Post, title, fileId) )
import Control.Monad.Free (Free (Pure, Free), foldFree, liftF)
import Data.List (find)
import API.Telegram
import Data.Maybe (isJust)
import Data.Generics.Labels ()
import Control.Lens ( (^?), (^.), _Just, Ixed (ix), (<.), At (at), ixAt )
import Control.Applicative
import Control.Monad.Free.Church (foldF)
import Control.Monad (when)

data HandlerEntry a
    = HandlerEntry {
    trigger :: String
    , handler :: Scenario a
    }

wrongOptionMessage = "Wrong option, try again"


handleFewWithGreeting :: [HandlerEntry b] -> String -> String -> Scenario b
handleFewWithGreeting entries greeting retryMsg = do
    eval $ SendWith $ sendTextNButtonsEntry greeting buttons 
    text <- expect anyText
    case ((text ==) . trigger) `find` entries of 
        Nothing -> do
            eval $ sendText retryMsg
            handleFewWithGreeting entries greeting retryMsg
        Just one -> handler one
    where
    buttons = map ((: []) . trigger) entries


expectFew :: Foldable t => t String -> Scenario (Maybe String)
expectFew list = do
    text <- expect anyText
    pure if text `elem` list then Just text else Nothing


constrExpectText :: Scenario String
constrExpectText = anyText `expectOrReply` "Text expected"


constrExpectPhoto :: Scenario String
constrExpectPhoto = anyPhoto `expectOrReply` "Photo expected"

expectOrReply :: (Update -> Maybe a) -> String -> Scenario a
expectOrReply pred failMsg = do 
    update <- expect Just
    case pred update of
        Nothing -> do
            evalReply failMsg
            expectOrReply pred failMsg
        Just res -> pure res

anyPhoto :: Update -> Maybe String
anyPhoto update 
    = update ^? #message
    . _Just . #photo 
    . _Just . ix 0 . #file_id

withChatId :: Update -> Maybe ChatId
withChatId update = update ^? #message . _Just . #from . #id

anyTextWithChatId :: Update -> Maybe (String, ChatId) 
anyTextWithChatId update =
    (,) <$> anyText update <*> withChatId update

anyText :: Update -> Maybe String
anyText update = update ^? #message . _Just . #text . _Just


evalReply :: String -> Scenario ()
evalReply = eval . sendText

checkIsHavePost = undefined 

post :: Scenario ()
post = do
    exists <- checkIsHavePost
    offerFew "It's your post settings" do
        onText "create" create
        onText "back" $ pure ()
        when exists do
            onText "edit" $ pure ()
            onText "show" $ pure ()
            onText "delete" $ pure ()

    where
    create = do
        evalReply "please enter title"
        (title, chatId) <- anyTextWithChatId `expectOrReply` "Text expected"

        evalReply "please send heading photo"
        fileId <- constrExpectPhoto
        
        evalReply "now send join link to you channel "
        link <- constrExpectText
        
        eval $ CreatePost $ Post title chatId fileId link
        evalReply "Ok! your post have created"

showPost :: AdvPost -> Scenario ()
showPost Post{..} = eval $ SendWith $ F.sendPhoto fileId title [["Like", "Dislike"], ["Back"]]

findS :: Scenario ()
findS = do
    post <- findRandPost
    maybe onAbsent onPresent post
    where
    onAbsent = do
        offerFew "There are no more post /any post yet :/" do 
            onText "Back" $ pure ()
            onText "Try again" findS
            

    onPresent post = do
        showPost post
        offerFew "What you think about this channel ?" do
            onText "Like" do
                eval $ LikePost post
                findS
            onText "Dislike" do
                eval $ DislikePost post
                findS
            onText "Back" $ pure ()
            


review = do
    pure ()

isTextMatchU :: String -> Update -> Bool
isTextMatchU text update = case textU update of
  Nothing -> False
  Just s -> text == s

branch `returnOn` word =
    returnIf (isTextMatchU word) branch do
        evalReply $ "returing from: " ++ word
        pure ()


data HandlerEntryF a
    = HandlerEntryF {
    triggerF :: String
    , handlerF :: Scenario () 
    , next :: a
    } 
    deriving Functor

type FreeHandler a = Free HandlerEntryF a


onText trigger branch = liftF $ HandlerEntryF trigger branch ()


offerFew :: String -> FreeHandler a -> Scenario a
offerFew = do
    --let buttons =
    undefined

lobby = do 
    offerFew "Lobby" do
        onText "post" do
            post `returnOn` "back"
        onText "find" findS
        onText "review" $ pure ()
    lobby

