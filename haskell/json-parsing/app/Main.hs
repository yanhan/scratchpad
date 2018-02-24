{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative (Alternative(empty))
import Data.Aeson
       ((.:), (.=), FromJSON(parseJSON), ToJSON(toEncoding, toJSON),
        Value(Object), decode, defaultOptions, encode, genericToEncoding,
        object)
import Data.Text (Text)
import GHC.Generics (Generic)

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
