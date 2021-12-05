module API.Telegram where

import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as M 
import Network.URI.Encode as N ( encode )
import qualified Data.String as S
import Data.List ( intercalate )
import GHC.Generics ()

openHTTPS :: String -> IO LB.ByteString
openHTTPS = HC.simpleHttp


token :: [Char]
token = "2094069209:AAHoBnp3rbASgqR4ZNzgN26MJZWT8jW9xX4"


data Request = 
    GetMe |
    GetUpdates

instance Show Request where
    show GetMe = "getMe"
    show GetUpdates = "getUpdates"

execTgGeneric :: [Char] -> [Char] -> IO LB.ByteString
execTgGeneric token x = openHTTPS $ "https://api.telegram.org/bot" ++ token ++ "/" ++ x

execSimpleTg :: Show a => [Char] -> a -> IO LB.ByteString
execSimpleTg token req = execTgGeneric token (show req)

pack :: [Char] -> LB.ByteString
pack = LB.pack

unpack :: LB.ByteString -> [Char]
unpack = LB.unpack

mapToArgs :: M.Map String String -> String
mapToArgs = encode . intercalate "&" . zip' . unzip . M.toList 
    where zip' (x, y) = zipWith (\a b -> a ++ "=" ++ b) x y 


execArgsTg :: Show a => String -> a -> M.Map String String -> IO LB.ByteString
execArgsTg token req args = openHTTPS $ "https://api.telegram.org/bot" ++ token ++ "/" ++ show req ++ "?" ++ mapToArgs args
