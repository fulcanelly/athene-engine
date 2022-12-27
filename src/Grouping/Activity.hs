module Grouping.Activity where



class ActivitiyDB db where 


data ActUpdate =
    Post {
        channelId :: Integer
        , time :: Int
    } |
    MemberCount {
        channelId :: Integer
        , count :: Integer
        , time :: Int
    }
{- may be needed to more precise channel activity classification
    PostViews {
       channelId :: Integer
       , postId :: Integer
       , time :: Int
       , count :: Int
    }
   
-}



-- should create new thread wich will handle activity updates
-- and based on them assign or update channel's act. category 
startWatching = undefined


