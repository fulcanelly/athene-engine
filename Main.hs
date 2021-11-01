{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}


module Main where

import Network.HTTP ( getRequest, getResponseBody, simpleHTTP ) 

openURL x = getResponseBody =<< simpleHTTP (getRequest x)


main :: IO ()
main = putStrLn "Hello, Haskell!"
