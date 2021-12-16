{-# LANGUAGE BlockArguments #-}

module Data.Logic where
import Control.FreeState
    ( anyText,
      expect,
      eval,
      replyText,
      Scenario,
      Command(CreatePost, AnswerWith),
      MessageEntry(TextNButtons) )
import Data.Posts ( AdvPost, AdvPostTemplate(Post) )
import Control.Monad.Free ()


data HandlerEntry a
    = HandlerEntry {
    trigger :: String
    , handler :: Scenario a
    }

wrongOptionMessage = "Wrong option, try again"

findOne :: (a -> Bool) -> [a] -> Maybe a
findOne pred list =
    let matches = filter pred list
    in if null matches then Nothing else Just $ head matches

isTriggerSameAs text = (text ==) . trigger




handleFewWithGreeting :: [HandlerEntry b] -> String -> String -> Scenario b
handleFewWithGreeting entries greeting retryMsg = do
    eval $ AnswerWith $ TextNButtons greeting buttons
    text <- expect anyText
    case findOne (isTriggerSameAs text) entries of 
        Nothing -> do
            eval $ replyText retryMsg
            handleFewWithGreeting entries greeting retryMsg
        Just one -> handler one
    where
    buttons = map ((: []) . trigger) entries

autoHandleFew :: String -> [HandlerEntry b] -> Scenario b
autoHandleFew greeting entries = handleFewWithGreeting entries greeting wrongOptionMessage

expectFew :: Foldable t => t String -> Scenario (Maybe String)
expectFew list = do
    text <- expect anyText
    pure if text `elem` list then Just text else Nothing


anyPhoto :: Num a => b -> Maybe a
anyPhoto = const $ Just 0

evalReply :: String -> Scenario ()
evalReply = eval . replyText

post :: Scenario ()
post = do
    autoHandleFew "It's your post settings" [
        HandlerEntry "create" create,
        HandlerEntry "back" $ pure ()
        ] 
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

find :: Scenario ()
find = do
    post <- fetchPost
    maybe onAbsent onPresent post
    where
    onAbsent = do 
        autoHandleFew "There are no more post /any post yet :/" [
            HandlerEntry "Back" $ pure (),
            HandlerEntry "Try again" find
            ] 

    onPresent post = do
        --eval $ ShowPost
        autoHandleFew "What you think about this channel ?" [
            HandlerEntry "Like" do
                -- eval LikePost post
                find,
            HandlerEntry "Dislike" do
                -- eval DislikePost post
                find,
            HandlerEntry "Back" $ pure ()
            ] 
    --eval $ ShowPost


review = do
    pure ()


lobby :: Scenario ()
lobby = do
    autoHandleFew "Lobby" [
        HandlerEntry "post" post,
        HandlerEntry "find" find,
        HandlerEntry "review" review
        ] 
    lobby