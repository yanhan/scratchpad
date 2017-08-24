#!/usr/bin/env stack
-- stack --resolver lts-9.0 --install-ghc runghc
import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>))

displayMaybe maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear
    | futureYear < birthYear = yearDiff birthYear futureYear
    | otherwise = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = yearDiff <$> readMaybe futureYearString <*> readMaybe birthYearString
    displayMaybe maybeAge
