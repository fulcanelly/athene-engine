{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Data.Logic where
import Control.FreeState as F

import Data.Posts ( AdvPost, AdvPostTemplate(Post, title, fileId, link) )
import Control.Monad.Free (Free (Pure, Free), foldFree, liftF)
import Data.List (find)
import API.Telegram
import Data.Maybe (isJust)
import Data.Generics.Labels ()
import Control.Lens ( (^?), (^.), _Just, Ixed (ix), (<.), At (at), ixAt )
import Control.Applicative
import Control.Monad.Free.Church (foldF)
import Control.Monad (when, void)


data HandlerEntry a
    = HandlerEntry {
    trigger :: String
    , handler :: Scenario a
    }

wrongOptionMessage = "Wrong option, try again"


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

showPost :: AdvPost -> String -> Scenario ()
showPost Post{..} msg = eval $ SendWith $ F.sendPhoto fileId caption [["Like", "Dislike"], ["Back"]]
    where caption = title <> "\n\n" <> link <> msg

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
        showPost post "\n\n\nWhat you think about this channel ?"
        handleFew do 
            onText "Like" do   
                eval $ LikePost post
            onText "Dislike" do
                eval $ DislikePost post
            onText "Back" $ pure ()


review = do
    pure ()

isTextMatchU :: String -> Update -> Bool
isTextMatchU text update = case textU update of
  Nothing -> False
  Just s -> text == s

branch `returnOn` word =
    returnIf (isTextMatchU word) branch do
        pure ()


type VoidHashBuilder a = HashBuilder a ()

onText :: String -> Scenario b -> VoidHashBuilder (Scenario b)
onText = putVal 

data HashBuilderF a next = PutValue String a next  
    deriving Functor 

type HashBuilder a = Free (HashBuilderF a)

putVal ::String -> a -> HashBuilder a ()
putVal k v = liftF $ PutValue k v ()

buildTable :: HashBuilder a b -> Map String a
buildTable (Pure next) = [] 
buildTable (Free (PutValue k v next)) = [(k,v)] `M.union` buildTable next

offerFew :: String -> VoidHashBuilder (Scenario ()) -> Scenario ()
offerFew greeting entry = do
    let table = buildTable entry
    eval $ SendWith $ sendTextNButtonsEntry greeting [M.keys table]
    text <- expect anyText
    runFoundOrWarn text table

handleFew :: VoidHashBuilder (Scenario ()) -> Scenario ()
handleFew entry = do
    text <- expect anyText
    runFoundOrWarn text $ buildTable entry

runFoundOrWarn text table =
    case text `M.lookup` table of 
        Nothing -> do
            evalReply "unknown option"
        Just scen -> scen

lobby :: Scenario ()
lobby = do 
    offerFew "Lobby" do
        onText "post" do
            post `returnOn` "Back"
        onText "find" do
            findS `returnOn` "Back"
        onText "review" review
    lobby



onPostLike :: Scenario a -> Int -> Scenario a
onPostLike continue count = do
    offerFew ("You got " <> show count <> " adv offers") do
        onText "Show" do
            review
            continue
        onText "Latter" do continue
        
    