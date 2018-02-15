{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.IO as TIO
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
       (defaultSettings, runSettings, setHost, setPort)
import Servant
       ((:>), (:<|>)(..), Application, Get, Handler, Post, Proxy(Proxy),
        ReqBody, Server, serve)
import Servant.API.ContentTypes (JSON, NoContent(NoContent), PlainText)

type OverallAPI = HealthCheckAPI
  :<|> UserAPI

overallAPI :: Proxy OverallAPI
overallAPI = Proxy


data User = User
  { name :: String
  , age :: Int
  , email :: String
  , birthDate :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

users :: [User]
users =
  [ User "Isaac Newton"  372  "isaac@newton.co.uk"  (fromGregorian 1683 3 1)
  , User "Albert Einstein"  136  "ae@mc2.org"  (fromGregorian 1905 12 1)
  ]

usersPostHandler :: User -> Handler NoContent
usersPostHandler u =
  let (year, month, day) = toGregorian $ birthDate u
  in liftIO $ do
    TIO.putStrLn $ "POST received: " <> pack (show u)
    TIO.putStrLn $ "year = " <> pack (show year) <>
      ", month = " <> pack (show month) <> ", day = " <> pack (show day)
    return NoContent

type UserAPI =
  -- GET /users
  "users" :> Get '[JSON] [User]
  -- POST /users
  -- this expects post data for a `User` in JSON form. For instance:
  --
  --     curl -XPOST -H 'Content-Type: application/json' -d '{"name": "Haskell Curry", "age": 118, "email": "haskell.curry@haskell.org", "birthDate": "12-09-1900"}'  http://127.0.0.1:8083/users
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

userServer :: Server UserAPI
userServer = return users
  :<|> usersPostHandler

userAPI :: Proxy UserAPI
userAPI = Proxy


-- GET /health_check
-- Notice that we the return type is `PlainText`
type HealthCheckAPI = "health_check" :> Get '[PlainText] Text

healthCheckServer :: Server HealthCheckAPI
healthCheckServer = return "OK"


-- This is how you support multiple API types
app :: Application
app = serve overallAPI $
  healthCheckServer :<|> userServer


port :: Int
port = 8083

-- This is how you can change the port and bind it to a specific interface
-- such as 127.0.0.1 instead of listening on all interfaces
main :: IO ()
main =
  let settings = defaultSettings & setPort port & setHost "127.0.0.1"
  in do
    liftIO $ TIO.putStrLn $ "Listening at port " <> pack (show port)
    runSettings settings app
