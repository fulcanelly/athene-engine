module Data.Logic where
import Control.FreeState
import Data.Posts


find = do
    pure ()

review = do
    pure ()


data HandlerEntry a
    = HandlerEntry {
    trigger :: String
    , handler :: Scenario a
    }

handleFew entries retryMsg = do
    text <- expect anyText
    let matches = filter ((text ==) . trigger )  entries
    if null matches then do
        eval $ replyText retryMsg
        handleFew entries retryMsg
    else do
        handler $ head matches

expectFew list = do
    text <- expect anyText
    if text `elem` list
    then pure $ Just text
    else do pure Nothing


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
        
lobby = do
    evalReply "you can [post / find / review ] "

    handleFew [
        HandlerEntry "post" post,
        HandlerEntry "find" find,
        HandlerEntry "review" review
        ] "wrong option, try again"
    lobby