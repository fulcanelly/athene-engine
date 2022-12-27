module Data.Review where


type ChannelId = Integer

type Reviewer = ChannelId 
type PostId = Integer


getUnratedPost :: Reviewer -> IO PostId
getUnratedPost = undefined 

likePost :: Reviewer -> PostId -> IO ()
likePost = undefined 


dislikePost :: Reviewer -> PostId -> IO ()
dislikePost = undefined 
