{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative (Alternative(empty))
import Data.Aeson
       ((.:), (.=), FromJSON(parseJSON), ToJSON(toEncoding, toJSON),
        Value(Number, Object, String), decode, defaultOptions, encode,
        genericToEncoding, object, withArray, withObject)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Scientific (floatingOrInteger, toBoundedInteger)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Vector (toList)

import Lib

-- Using the `DeriveGeneric` language extension
data Car = Car { model :: Text, year :: Int } deriving (Generic, Show)

instance FromJSON Car

-- According to https://haskell-lang.org/library/aeson
-- This is faster than using the default `ToJSON` implementation when
-- using the `DeriveGeneric` language extension
instance ToJSON Car where
  toEncoding = genericToEncoding defaultOptions


-- Custom encoding and decoding by defining own parseJSON and toJSON functions
data Food = Food { foodName :: Text, price :: Double } deriving (Eq, Show)

instance FromJSON Food where
  parseJSON (Object v) = Food <$> v .: "name" <*> v .: "price"
  parseJSON _ = empty

instance ToJSON Food where
  toJSON (Food n p) = object [ "name" .= n, "price" .= p ]


-- From https://artyom.me/aeson
-- This tutorial shows how to define parsers that work on `Value`s
-- Parser is an instance of MonadFail, which is why we can use the
-- `fail` function for indicating various failures
parseTuple :: Value -> Parser (Int, Text)
parseTuple = withObject "tuple of (Int, String)" f
  where
    f obj = do
      fieldOne <- case HM.lookup "one" obj of
                    Just (Number x) ->
                      case floatingOrInteger x of
                        Right _ ->
                          case (toBoundedInteger x :: Maybe Int) of
                            Just y -> return y
                            _ -> fail $ show x ++ " is too big to convert to an Int"
                        _ -> fail "expected an Int but got a Float"
                    Just _ -> fail "expected an Int"
                    _ -> fail "no field 'one"

      fieldTwo <- case HM.lookup "two" obj of
                    Just (String x) -> return x
                    Nothing -> fail "no field two"

      return (fieldOne, fieldTwo)

-- This uses the built-in FromJSON instances to simplify parsing
parseTuple' :: Value -> Parser (Int, Text)
parseTuple' = withObject "tuple of (Int, String)" f
  where
    f obj = do
      fieldOne <- case HM.lookup "one" obj of
                    Just x -> parseJSON x
                    _ -> fail "no field 'one"

      fieldTwo <- case HM.lookup "two" obj of
                    Just x -> parseJSON x
                    Nothing -> fail "no field two"

      return (fieldOne, fieldTwo)

-- Further simplify using (.:) operator
parseTuple'' :: Value -> Parser (Int, Text)
parseTuple'' = withObject "tuple of (Int, String)" f
  where
    f obj = do
      fieldOne <- obj .: "one"
      fieldTwo <- obj .: "two"
      return (fieldOne, fieldTwo)

parseArray :: Value -> Parser [(Int, Text)]
parseArray = withArray "array of (Int, String)" (mapM parseTuple'' . toList)


main :: IO ()
main = do
  putStrLn $ "Encode: " ++ show (encode Car {model = "T", year = 1940} )
  let burger = Food {foodName = "Burger", price = 6.35}
  putStrLn $ "Encode burger: " ++ show (encode burger)
  putStrLn $ "Decode (Encode burger) == burger = " ++
    show (decode (encode burger) == Just burger)
  putStrLn $ "Decode Food: " ++
    show (decode "{\"name\": \"Pork Belly\", \"price\": 1.50}" :: Maybe Food)
  putStrLn $ "Decode Food (fail): " ++
    show (decode "{\"foodName\": \"Chicken Wings\", \"price\": 5.70}" :: Maybe Food)
  -- Working with arbitrary JSON data on its AST
  let someJson = decode "{\"hero\": true, \"attributes\":[{\"strength\": 39}, {\"agility\": 100.5}]}" :: Maybe Value
  print someJson
  print $ parseMaybe parseTuple =<< decode  "{\"one\": 5, \"two\": \"Sever\"}"
  print $ parseMaybe parseTuple' =<< decode  "{\"one\": 5, \"two\": \"Sever\"}"
  print $ parseMaybe parseTuple'' =<< decode  "{\"one\": 5, \"two\": \"Sever\"}"
  print $ parseMaybe parseArray =<< decode
    "[{\"one\": 7, \"two\": \"Up\"}, {\"one\": 33, \"two\": \"Sprint\"}]"
