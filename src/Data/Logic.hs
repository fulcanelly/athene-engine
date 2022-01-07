{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Logic where
import Control.FreeState as F

import Data.Posts
import Control.Monad.Free (Free (Pure, Free), foldFree, liftF)
import Data.List (find)
import API.Telegram
import Data.Maybe
import Data.Generics.Labels ()
import Control.Lens ( (^?), (^.), _Just, Ixed (ix), (<.), At (at), ixAt, makeLenses, set)
import Control.Applicative
import Control.Monad.Free.Church (foldF)
import Control.Monad (when, void, forever)
import Data.Map as M
import GHC.Exts (IsList)
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Prelude as P


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


sendWithKb :: String -> [[String]] -> Scenario ()
sendWithKb text = eval . SendWith . sendTextNButtonsEntry text

genKb :: Map e a -> [[e]]
genKb = chunksOf 3 . reverse . M.keys 

offerFew :: String -> VoidHashBuilder (Scenario a) -> Scenario a
offerFew greeting entry = do
    let table = buildTable entry
    sendWithKb greeting $ genKb table
    text <- expect anyText
    runFoundOrWarnWithLoop text table (offerFew greeting entry)

handleFew :: VoidHashBuilder (Scenario a) -> Scenario a
handleFew entry = do
    text <- expect anyText
    runFoundOrWarnWithLoop text (buildTable entry) (handleFew entry)


runFoundOrWarnWithLoop text table again =
    case text `M.lookup` table of
        Nothing -> do
            sendWithKb "Не знаю о чем вы" $genKb table
            again
        Just scen -> scen



data MapperBuilderF mapper next
     = Prompt mapper next
    deriving Functor

type FreeMapperBuilder a b = Free (MapperBuilderF (a (b -> b)))

prompt :: (Monad f) => (a -> b -> b) -> f a -> FreeMapperBuilder f b ()
prompt m s = liftF $ Prompt (m <$> s) ()

promptM :: (Monad f) => (a -> b -> b) -> f (Maybe a) -> FreeMapperBuilder f b ()
promptM m s = liftF $ Prompt (maybe P.id m <$> s) ()

toMapper :: (Monad f) => FreeMapperBuilder f b c -> f (b -> b)
toMapper (Pure next) = pure P.id
toMapper (Free (Prompt mapper next)) = (.) <$> mapper <*> toMapper next

-- update parsers

expectFew :: Foldable t => t String -> Scenario (Maybe String)
expectFew list = do
    text <- expect anyText
    pure if text `elem` list then Just text else Nothing


constrExpectText :: Scenario String
constrExpectText = anyText `expectOrReply` "Ожидался текст"


constrExpectPhoto :: Scenario String
constrExpectPhoto = anyPhoto `expectOrReply` "Ожидалось фото"

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
evalReply = eval . SendWith . sendText

checkIsHavePost :: Free ScenarioF Bool
checkIsHavePost = isJust <$> loadMyPost

sendWithButtons :: String -> [[String]] -> Scenario ()
sendWithButtons a = eval . SendWith . sendTextNButtonsEntry a

-- actual behavior 

post :: Scenario ()
post = do
    exists <- checkIsHavePost
    offerFew "Что сделать с вашим рекламным постом?" do
        if exists then do
            onText_ "Редактировать" edit 
            onText_ "Показать" show
            onText "Удалить" do 
                res <- delete 
                if res then pure () else post 
        else
            onText_ "Создать" create
        onText "Назад" $ pure ()
    where
    create = do
        evalReply "Введите заголовок"
        (title, chatId) <- anyTextWithChatId `expectOrReply` "Ожидался текст"

        evalReply "Отправьте фото поста"
        fileId <- constrExpectPhoto

        evalReply "Теперь отправтье ссылку на ваш канал"
        link <- constrExpectText

        eval $ CreatePost $ Post title chatId fileId link
        evalReply "Все! теперь ваш пост создан"

    show = do
        Post{..} <- fromJust <$> loadMyPost
        eval $ SendWith $ F.sendPhoto _fileId (_title <> "\n" <> _link <> "\n\nЧто делать?") [["Редактировать"],["Назад"]]
        handleFew do
            onText "Редактировать" do
                edit
            onText "Назад" do
                pure ()
    edit = do
        original @Post {..} <- fromJust <$> loadMyPost

        update <- toMapper do

           -- promptM (set fileId) do
             --   pure $ Just ""

            prompt (set title) do
                sendWithButtons "Введите новый заголовок" [[_title]]
                expect anyText

            prompt (set link) do
                sendWithButtons "Введите новую ссылку" [[_link]]
                expect anyText

        let updated = update original

        when (updated /= original) (eval $ UpdatePost updated)

        evalReply "Ваш пост был обновлен"
    delete = do 
        offerFew "Вы уверены что хотите удалить ваш пост ?" do
            onText "Да" do
                eval DeleteMyPost
                pure True
            onText "Назад" do
                pure False
    onText_ text scenario = 
        onText text do 
            scenario 
            post


showPost :: AdvPost -> String -> Scenario ()
showPost Post{..} msg = eval $ SendWith $ F.sendPhoto _fileId caption [["Лайк", "Дизлайк"], ["Назад"]]
    where caption = _title <> "\n\n" <> _link <> msg

findS :: Scenario ()
findS = do
    post <- findRandPost
    maybe onAbsent onPresent post
    where
    onAbsent = do
        offerFew "Постов больше или пока нет :/" do 
            onText "Назад" $ pure ()
            onText "Попробовать еще раз" findS
            

    onPresent post = do
        showPost post "\n\n\nЧто вы думаете про этот канал ?"
        handleFew do 
            onText "Нравится" do   
                eval $ LikePost post
                findS
            onText "Ненравится" do
                eval $ DislikePost post
                findS
            onText "Назад" $ pure ()


review = do
    pure ()

isTextMatchU :: String -> Update -> Bool
isTextMatchU text update = case textU update of
  Nothing -> False
  Just s -> text == s

returnOn :: Scenario () -> String -> Scenario ()
branch `returnOn` word =
    returnIf (isTextMatchU word) branch do
        pure ()

selectLanguage
    = offerFew "Выберите язык" do 
        onText "🇬🇧" do
            evalReply "Английски пока не поддерживается"

        onText "🇷🇺" do
            evalReply "Язык выбран"

            
introduce = do
    offerFew "What is it? Think about this bot as your personal channel adverts manager \n\n \
    \It will help you find similar channels to work with \n\n\
    \Currently it's in testing mode so don't expect to much from it, good luck ;)" do 
        onText "Ok" do pure ()  
    
lobby :: Scenario ()
lobby = do
    clean 1
    have <- checkIsHavePost
    offerFew "Главное меню" do
        onText "Мой пост" do
            post -- `returnOn` "Back"
        when have do 
            onText "Найти" findS 
     --   onText "review" review
    lobby

-- hooks


startBot = do 
    expect Just
    selectLanguage 
    introduce
    lobby
    
onPostLike :: Scenario a -> Int -> Scenario a
onPostLike continue count = do
    offerFew ("You got " <> show count <> " adv offers") do
        onText "Show" do
            evalReply "showing"
            review
        onText "Latter" do pure ()    
    continue


startOnPostLike count = do
    offerFew ("You got " <> show count <> " adv offers") do
        onText "Show" do
            evalReply "showing"
        onText "Latter" do pure ()
    lobby