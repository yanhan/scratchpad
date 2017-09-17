#!/usr/bin/env stack
-- stack script --resolver lts-8.12 --package conduit --package conduit-combinators --package conduit-extra --package text

{-# LANGUAGE OverloadedStrings #-}

import Conduit (decodeUtf8C, lineC, peekForeverE)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Char
import Data.Conduit ((.|), await, runConduitRes)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- About
-- =====
-- Example code of using Conduit to process a file one line at a time
-- The `lineC` expects a consumer / sink. So we can't pass (omapCE id) or
-- similar to it.
-- Therefore, we have to come up with our own consumer to do the processing.
--
-- In this case, the consumer simply outputs the capitalized version of the
-- line.
-- This should be sufficient to show that you can do some very interesting
-- processing on the line.

processLineByLine :: FilePath -> IO ()
processLineByLine path_to_file = do
  runConduitRes $ sourceFile path_to_file .| decodeUtf8C .| peekForeverE (lineC consumer)
  where
    consumer = do
      mx <- await
      case mx of
        Just x -> liftIO $ TIO.putStrLn $ T.map (Data.Char.toUpper) x

        -- NOTE: We need the `TIO.putStrLn ""` part to output a newline for
        -- every actual empty line in the file. Otherwise all the non-empty
        -- lines that are separated by newlines will be clumped together.
        --
        -- Do note that both these work:
        -- Nothing -> (liftIO $ TIO.putStrLn "") >> return ()
        -- Nothing -> liftIO (TIO.putStrLn "" >> return ())
        Nothing -> liftIO $ TIO.putStrLn "" >> return ()

main :: IO ()
main = processLineByLine "./conduit-line-by-line.hs"
