module Main where

import Conduit
       (dropWhileC, foldC, mapC, mapM_C, sinkList, sumC, takeC, yieldMany)
import Data.Conduit ((.|), ConduitM, runConduit, runConduitPure)
import Data.Monoid (Product(..))
import Data.Text (Text, pack)

import Lib

-- Using Conduit as monad (example 1)
sourceConduitMonad :: Monad m => ConduitM i Int m ()
sourceConduitMonad = do
  yieldMany [3..9]
  yieldMany [30..35]

-- Using Conduit as monad (example 2)
sinkConduitMonad :: Monad m => ConduitM Int o m (Text, Int, Product Int)
sinkConduitMonad = do
  s <- takeC 4 .| mapC (pack . show) .| foldC
  sum5 <- takeC 5 .| sumC
  prod <- mapC Product .| foldC
  return (s, sum5, prod)

-- Write sinkConduitMonad without using do notation
sinkConduitMonadAsApplicative :: Monad m => ConduitM Int o m (Text, Int, Product Int)
sinkConduitMonadAsApplicative =
  (,,) <$> (takeC 4 .| mapC (pack . show) .| foldC)
       <*> (takeC 5 .| sumC)
       <*> (mapC Product .| foldC)

-- Using Conduit as monad (neither source nor sink)
intermediateConduitMonad :: Monad m => ConduitM Int Int m ()
intermediateConduitMonad = do
  takeC 4 .| mapC (*2)
  mapC (+7)

-- Using Conduit as monad - first 3, next 3, final 3, discard (exercise)
interConduitMFirst3Next3Last3 :: Monad m => ConduitM Int Int m ()
interConduitMFirst3Next3Last3 = do
  takeC 3 .| mapC (\x -> x - 1)
  takeC 3 .| mapC (+1)
  takeC 3 .| mapC (*3)
  dropWhileC (const True)

main :: IO ()
main = do
  print "Using Conduit as monad (example 1)"
  runConduit $ sourceConduitMonad .| mapM_C print
  print "Using Conduit as monad (example 2)"
  print $ runConduitPure $ yieldMany [2..17] .| sinkConduitMonad
  print "Write sinkConduitMonad without using do notation"
  print $ runConduitPure $ yieldMany [2..17] .| sinkConduitMonadAsApplicative
  print "Using Conduit as monad (neither source nor sink)"
  print $ runConduitPure $ yieldMany [3..8] .| intermediateConduitMonad .| sinkList
  print "Using Conduit as monad - first 3, next 3, final 3, discard (exercise)"
  print $ runConduitPure $
    yieldMany [50..70] .| interConduitMFirst3Next3Last3 .| sinkList
