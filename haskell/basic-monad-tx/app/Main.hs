module Main where

import Lib

import Control.Exception.Base (bracket)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import System.IO (hFlush, hGetEcho, hSetEcho, stdin, stdout)

getName :: MaybeT IO String
getName = MaybeT $ do
  putStr "Enter username: "
  hFlush stdout
  username <- getLine
  if username == "myuser"
     then return $ Just username
     else return Nothing

getPassword :: MaybeT IO String
getPassword = MaybeT $ do
  putStr "Enter password: "
  hFlush stdout
  -- Don't echo user's password
  password <- bracket
    (hGetEcho stdin)
    (hSetEcho stdin)
    (const $ hSetEcho stdin False >> getLine)
  if password == "mypassword"
     then return $ Just password
     else return Nothing

-- We learnt how to chain up all the MaybeT from
-- https://mmhaskell.com/blog/2017/3/6/making-sense-of-multiple-monads
getUserCredentials :: MaybeT IO (String, String)
getUserCredentials = do
  -- This is not the best use case of MaybeT because the Maybe monad will cause
  -- the `getPassword` to not run if the username is incorrect
  username <- getName
  password <- getPassword
  return (username, password)

main :: IO ()
main = do
  mb_creds <- runMaybeT getUserCredentials
  case mb_creds of
    Just _ -> putStrLn "Correct username and password!"
    Nothing -> putStrLn "Incorrect username and/or password"
