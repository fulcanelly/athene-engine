{-# LANGUAGE BlockArguments #-}

module Data.Logic where
import Control.FreeState
import Data.Posts
import Control.Monad.Free


data HandlerEntry a
    = HandlerEntry {
    trigger :: String
    , handler :: Scenario a
    }

wrongOptionMessage = "Wrong option, try again"


handleFew entries retryMsg = do
    text <- expect anyText
    let matches = filter ((text ==) . trigger ) entries
    if null matches then do
        eval $ replyText retryMsg
        handleFew entries retryMsg
    else do
        handler $ head matches

handleFewWithGreeting entries greeting retryMsg = undefined

expectFew list = do
    text <- expect anyText
    pure if text `elem` list then Just text else Nothing


anyPhoto = const $ Just 0

evalReply = eval . replyText

post = do
    evalReply "you can [create / edit / delete / show] your post"
    handleFew [
        HandlerEntry "create" create,
        HandlerEntry "back" $ pure ()
        ] "wrong option, try again"
    where
    create = do
        evalReply "please enter title"
        title <- expect anyText
        evalReply "please send heading photo"
        fileId <- expect anyPhoto
        evalReply "now send join link to you channel "
        link <- expect anyText
        eval $ CreatePost $ Post title undefined fileId link
        evalReply "Ok! your post have created"


fetchPost :: Scenario (Maybe AdvPost)
fetchPost = undefined

find = do
    post <- fetchPost
    maybe onAbsent onPresent post
    where
    onAbsent = do
        evalReply "There are no any post yet :/"
        handleFew [
            HandlerEntry "Back" $ pure (),
            HandlerEntry "Try again" find
            ] "Wrong option, try again"

    onPresent post = do
        --eval $ ShowPost
        handleFew [
            HandlerEntry "like" do
                -- eval LikePost post
                find,
            HandlerEntry "dislike" do
                -- eval DislikePost post
                find,
            HandlerEntry "back" $ pure ()
            ] "wrong option, try again"
    --eval $ ShowPost


review = do
    pure ()


lobby = do
    evalReply "you can [post / find / review ] "

    handleFew [
        HandlerEntry "post" post,
        HandlerEntry "find" find,
        HandlerEntry "review" review
        ] "wrong option, try again"
    lobby