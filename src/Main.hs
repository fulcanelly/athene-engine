{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

{-# OPTIONS_GHC -Wall #-}


module Main where

import Network.HTTP ( getRequest, getResponseBody, simpleHTTP ) 

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)


main :: IO ()
main = putStrLn "Hello, Haskell!"
