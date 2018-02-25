{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (Alternative(empty))
import Control.Monad (guard)
import Data.Aeson
       ((.:), (.:?), (.=), (.!=), FromJSON(parseJSON),
        ToJSON(toEncoding, toJSON), Value(Number, Object, String), decode,
        defaultOptions, encode, genericToEncoding, object, withArray,
        withObject)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Foldable (asum)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Scientific (floatingOrInteger, toBoundedInteger)
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Data.Vector (toList)
import Text.Read (readMaybe)

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

-- Optional field in the JSON representation
data Book = Book { title :: Text, author :: Text, bookPrice :: Double }
  deriving Show

instance FromJSON Book where
  parseJSON = withObject "Book" (\v -> do
    title <- v .: "title"
    author <- v .: "author"
    -- Default to $10 for bookPrice if it is not present
    bookPrice <- v .:? "bookPrice" .!= 10
    -- Below is a use of the RecordWildCards extension
    return Book{..})

instance ToJSON Book where
  -- Below is a use of the RecordWildCards extension
  toJSON Book{..} = object [ "title" .= title
                           , "author" .= author
                           , "bookPrice" .= bookPrice
                           ]

-- Slightly more complicated parsing rules
data Software = Software { softwareName :: Text, softwareVersion :: Int } deriving (Show)

instance FromJSON Software where
  parseJSON = withObject "Software" (\o -> do
    softwareName <- o .: "softwareName"
    {--
       Priority:
       - `softwareVersion` field
       - `version` field as a string
       - if none of the above is found and `softwareName` is "PooPoo", then
         use -2 as default value
    --}
    softwareVersion <- asum [ o .: "softwareVersion"
                            , do ver <- o .: "version"
                                 case readMaybe ver of
                                   Just v -> return v
                                   _ -> fail "expected an Int"
                            , do guard (softwareName == "PooPoo")
                                 return $ -2
                            ]
    return Software{..})

instance ToJSON Software where
  toJSON Software{..} = object [ "softwareName" .= softwareName
                               , "version" .= softwareVersion
                               ]

-- Types with multiple constructors
data House = HDB { block :: Int, zipCode :: Int }
           | Condo { condoName :: Text, zipCode :: Int }
           deriving Show

instance FromJSON House where
  parseJSON = withObject "HDB or Condo" $ \o ->
    asum [ HDB <$> o .: "block" <*> o .: "zipCode"
         , Condo <$> o .: "condoName" <*> o .: "zipCode"
         ]

instance ToJSON House where
  toJSON HDB{..} = object [ "block" .= block, "zipCode" .= zipCode ]
  toJSON Condo{..} = object [ "condoName" .= condoName, "zipCode" .= zipCode ]


-- Convert nested JSON object (received as a ByteString) into single layer
-- Haskell datatype.
-- Suppose the received ByteString has this format:
--
--     {
--         "name": "spice name",
--         "country": {
--             "name": "country name",
--             "climate": "country climate"
--         }
--     }
-- and we want to decode such ByteStrings to the Spice datatype below
data Spice =
  Spice { spiceName :: Text
        , countryOfSpice :: Text
        , climateOfSpice :: Text
        } deriving Show

instance FromJSON Spice where
  parseJSON = withObject "Spice" $ \o -> do
    spiceName <- o .: "name"
    spiceCountry <- o .: "country"
    countryOfSpice <- spiceCountry .: "name"
    climateOfSpice <- spiceCountry .: "climate"
    return Spice{..}

-- JSON with unknown field names.
-- Suppose we have the following data, where the keys are unknown but they
-- follow some kind of structure:
--
--     {
--         "brandOne": {
--             "productOne": 1,
--             "productTwo": 7
--         },
--         "brandTwo": {
--             "productThree": 12
--         }
--     }
--
-- We want to parse this JSON into a list of the following:
data ProductLine =
  ProductLine { productLineName :: Text
              , productSales :: [(Text, Int)]
              } deriving (Show)
-- More precisely, this:
-- [ Product "brandOne" [("productOne", 1), ("productTwo", 7)]
-- , Product "brandTwo" [("productThree", 12)]
-- ]

parseProductLines :: Value -> Parser [ProductLine]
parseProductLines = withObject "ProductLines" $ \o ->
  -- Convert the Object (which is a strict HashMap) into a Haskell List
  for (HM.toList o) $ \(productLineName, psalesObj) -> do
    -- productSales :: [(Text, Int)]
    productSales <- HM.toList <$> parseJSON psalesObj
    return ProductLine{..}


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
  print (decode "{\"title\": \"Game it\", \"author\": \"Bryan S\", \"bookPrice\": 6.99}" :: Maybe Book)
  print (decode "{\"title\": \"Three Little Dots\", \"author\": \"Will Dragon\"}" :: Maybe Book)
  print (decode "{\"softwareName\": \"vim\", \"softwareVersion\": 5}" :: Maybe Software)
  print (decode "{\"softwareName\": \"emacs\", \"version\": \"8\"}" :: Maybe Software)
  print (decode "{\"softwareName\": \"PooPoo\"}" :: Maybe Software)
  print (decode "{\"block\": 100, \"zipCode\": 171100}" :: Maybe House)
  print (decode "{\"condoName\": \"The Lake\", \"zipCode\": 171564}" :: Maybe House)
  print (decode "{\"name\": \"Chili\", \"country\": {\"name\": \"Thailand\", \"climate\": \"tropical\"}}" :: Maybe Spice)
  print (parseMaybe parseProductLines =<<
    decode "{\"Hokey\": {\"Dokey\": 5, \"Pricky\": 10}, \"Green\": {\"Barry\": 4}}")
