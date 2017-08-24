#!/usr/bin/env stack
-- stack --resolver lts-9.0 --install-ghc runghc
import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear
yourHelperFunction f x y = abs $ f x y

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = yourHelperFunction yearDiff <$> readMaybe futureYearString <*> readMaybe birthYearString
    displayAge maybeAge
