{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.IO as TIO
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
       (defaultSettings, runSettings, setHost, setPort)
import Servant (Proxy(Proxy))
import Servant.API ((:>), (:<|>)(..))
import Servant.API.Capture (Capture)
import Servant.API.ContentTypes (JSON, NoContent(NoContent), PlainText)
import Servant.API.Header (Header)
import Servant.API.QueryParam (QueryParam)
import Servant.API.Raw (Raw)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.API.ReqBody (ReqBody)
import Servant.API.Verbs (Get, Post)
import Servant.Server
       (Application, Handler, ServantErr(errBody), Server, err500, serve)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)

type OverallAPI = HealthCheckAPI
  :<|> UserAPI
  :<|> CountriesAPI
  :<|> UserAgentAPI
  :<|> StaticFilesAPI

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

-- The `NoContent` is how we indicate that an endpoint does not return anything
usersPostHandler :: User -> Handler NoContent
usersPostHandler u =
  let (year, month, day) = toGregorian $ birthDate u
  in liftIO $ do
    TIO.putStrLn $ "POST received: " <> pack (show u)
    TIO.putStrLn $ "year = " <> pack (show year) <>
      ", month = " <> pack (show month) <> ", day = " <> pack (show day)
    return NoContent

usersOfAge :: Maybe Int -> Handler [User]
usersOfAge maybeAge =
  case maybeAge of
    Just a -> return $ filter (\user -> age user == a) users
    Nothing -> return []

type UserAPI =
  -- GET /users
  "users" :> Get '[JSON] [User]
  -- POST /users
  -- this expects post data for a `User` in JSON form. For instance:
  --
  --     curl -XPOST -H 'Content-Type: application/json' -d '{"name": "Haskell Curry", "age": 118, "email": "haskell.curry@haskell.org", "birthDate": "1900-09-12"}'  http://127.0.0.1:8083/users
  --
  -- The `NoContent` is how we indicate that an endpoint does not return anything
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] NoContent
  -- Query parameter and additional static path fragment
  -- curl -vvv 'http://127.0.0.1:8003/users/qq?age=136'
  :<|> "users" :> "qq" :> QueryParam "age" Int :> Get '[JSON] [User]

userServer :: Server UserAPI
userServer = return users
  :<|> usersPostHandler
  :<|> usersOfAge

userAPI :: Proxy UserAPI
userAPI = Proxy


-- GET /health_check
-- Notice that we the return type is `PlainText`
type HealthCheckAPI = "health_check" :> Get '[PlainText] Text

healthCheckServer :: Server HealthCheckAPI
healthCheckServer = return "OK"


data Country = Country
  { countryName :: Text
  , countryCode :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Country
instance ToJSON Country

countries :: [Country]
countries =
  [ Country "China"  "CN"
  , Country "Malaysia"  "MY"
  ]

type CountriesAPI = "countries" :> "list" :> Get '[JSON] [Country]
  :<|> "countries" :> Capture "countrycode" Text :> Get '[JSON] (Maybe Country)
  :<|> "countries" :> "err" :> Capture "x" Text :> Get '[JSON] Country
  :<|> "countries_add_header" :> Get '[JSON] (Headers '[Header "Sentry-Duty" Int] Country)

getCountryByCode :: Text -> Handler (Maybe Country)
getCountryByCode c = return $
  listToMaybe $ filter (\country -> countryCode country == c) countries

-- This shows how you can throw an error inside the Handler monad
getCountryPerhapsError :: Text -> Handler Country
getCountryPerhapsError x =
  if x == "CN"
     then return $ Country "China"  "CN"
     else throwError $ err500 { errBody = "A custom error. You can change this!" }

getCountryAddHeaderHandler :: Handler (Headers '[Header "Sentry-Duty" Int] Country)
getCountryAddHeaderHandler = return $ addHeader 47 $ Country "Netherlands"  "NL"

countriesServer :: Server CountriesAPI
countriesServer = return countries
  :<|> getCountryByCode
  :<|> getCountryPerhapsError
  :<|> getCountryAddHeaderHandler

countriesAPI :: Proxy CountriesAPI
countriesAPI = Proxy


-- This part shows how you can extract the value of a HTTP Header
type UserAgentAPI = "user_agent" :> Header "User-Agent" Text :> Get '[JSON] Text

userAgentAPI :: Proxy UserAgentAPI
userAgentAPI = Proxy

userAgentServer :: Server UserAgentAPI
userAgentServer userAgent =
  case userAgent of
    Just s -> do
      liftIO . TIO.putStrLn $ "The user's user-agent is \"" <> s <> "\""
      return s
    Nothing -> return "The user did not specify the User-Agent header"


-- Serve static files in a directory
type StaticFilesAPI = "fixed_mindset" :> Raw

staticFilesAPI :: Proxy StaticFilesAPI
staticFilesAPI = Proxy

staticFilesServer :: Server StaticFilesAPI
staticFilesServer = serveDirectoryWebApp "fixed_mindset"


-- This is how you support multiple API types
app :: Application
app = serve overallAPI $
  healthCheckServer
  :<|> userServer
  :<|> countriesServer
  :<|> userAgentServer
  :<|> staticFilesServer


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
