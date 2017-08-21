{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^.), (^?), (^..), (.~), Identity, ix, to)
import Data.Aeson (decode)
import Data.Aeson.Lens (_Number, _String, key, nth, values)
import Data.Function ((&))
import Data.Maybe (listToMaybe, maybe)
import Data.Scientific (toRealFloat)
import Network.Wreq (defaults, getWith, param, responseBody)
import Numeric (showFFloat)
import Data.Text (Text, pack, unpack)
import Data.Time (ZonedTime, addUTCTime, parseTimeM)
import Data.Time.Lens (days, hours, minutes, months, years)
import Data.Time.LocalTime (TimeZone(TimeZone), utcToZonedTime, zonedTimeToUTC)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath.Posix ((</>))
import System.Locale.Read (getLocale, knownTimeZones)
import Text.Parsec (Parsec, Stream, char, count, digit, parse)
import Text.Parsec.Prim (ParsecT)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO as TIO

import Lib

fahrenheit_to_celcius :: Fractional a => a -> a
fahrenheit_to_celcius fah = (fah - 32) * 5 / 9

example_response = "[{\"DateTime\":\"2017-08-20T17:00:00+08:00\",\"EpochDateTime\":1503219600,\"WeatherIcon\":17,\"IconPhrase\":\"Partly sunny w/ t-storms\",\"IsDaylight\":true,\"Temperature\":{\"Value\":88.0,\"Unit\":\"F\",\"UnitType\":18},\"PrecipitationProbability\":52,\"MobileLink\":\"http://m.accuweather.com/en/sg/singapore/300597/hourly-weather-forecast/300597?day=1&lang=en-us\",\"Link\":\"http://www.accuweather.com/en/sg/singapore/300597/hourly-weather-forecast/300597?day=1&hbhhour=17&lang=en-us\"}]" :: BL.ByteString

datetime_parser :: (Stream s Identity Char) => Parsec s u [Char]
datetime_parser = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  char 'T'
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  char ':'
  second <- count 2 digit
  return $ concat [year, "-", month, "-", day, "T", hour, ":", minute, ":", second]

report_weather response_body datetime_string_with_zone datetime_string = do
    timeLocale <- getLocale $ Just "en_SG.UTF-8"
    let timeLocale' = timeLocale { knownTimeZones = [sg_timezone] }
    end_zonedtime <- parseTimeM True timeLocale' "%Y-%m-%dT%H:%M:%S%z" $
        unpack datetime_string_with_zone :: IO ZonedTime
    let end_utctime = zonedTimeToUTC end_zonedtime
    let start_utctime = addUTCTime (realToFrac (-3600)) end_utctime
    let start_zonedtime = utcToZonedTime sg_timezone start_utctime
    putStrLn $ "Weather forecast from " `mappend`
        (show (start_zonedtime ^. hours) `mappend` ":00") `mappend`
        " to " `mappend`
        (show (end_zonedtime ^. hours) `mappend` ":00")

    putStr "Summary: "
    maybe
        (putStrLn "Error obtaining summary from response_body")
        TIO.putStrLn
        (listToMaybe $ response_body ^.. nth 0 . key "IconPhrase" . _String)

    putStr "Temperature: "
    maybe
        (putStrLn "Error obtaining temperature from response_body")
        (\t -> putStrLn $
            (showFFloat (Just 2) (fahrenheit_to_celcius t) "") `mappend`
            " degrees Celcius"
        )
        (listToMaybe $ response_body ^.. nth 0 . key "Temperature" . key "Value" . _Number . to toRealFloat)

    putStr "Probability of raining: "
    maybe
        (putStrLn "Error retrieving probability from response_body")
        (\p -> putStrLn $ showFFloat (Just 2) p "")
        (listToMaybe $ response_body ^.. nth 0 . key "PrecipitationProbability" . _Number . to toRealFloat)

    maybe
        (return ())
        (\url -> TIO.putStrLn $ "For more information, please visit " `mappend` url)
        (listToMaybe $ response_body ^.. nth 0 . key "Link" . _String)
    where
        sg_timezone = TimeZone 480 False "+0800"

main :: IO ()
main = do
    -- Read in API key
    cur_dir <- getCurrentDirectory
    let path_to_config_file = cur_dir </> "config"
    config_file_exists <- doesFileExist path_to_config_file
    if config_file_exists
       then do
           api_key <- fmap pack $ readFile path_to_config_file
           let opts = defaults & param "apikey" .~ [api_key]
           r <- getWith opts "http://dataservice.accuweather.com/forecasts/v1/hourly/1hour/300597"
           maybe
               (putStrLn "Response has no body")
               (\r_body ->
                   maybe
                       (TIO.putStrLn "Error extracting `DateTime` from response")
                       (\datetime_with_timezone ->
                           either
                               (\_ -> TIO.putStrLn $
                                   "Error parsing datestring `" `mappend`
                                       datetime_with_timezone  `mappend`
                                       "`"
                               )
                               (report_weather r_body datetime_with_timezone)
                               (parse datetime_parser "AccuWeather API"
                                   (unpack datetime_with_timezone)
                               )
                       )
                       (listToMaybe $ r_body ^.. nth 0 . key "DateTime" . _String)
               )
               (r ^? responseBody)
        else do
            putStrLn $
                "Config file `" `mappend`
                path_to_config_file `mappend`
                "` does not exist. Please create it. Its contents should be a single line containing the Accuweather API key."
            exitWith $ ExitFailure 1
