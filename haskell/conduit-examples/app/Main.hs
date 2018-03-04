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
  -- yields 3 to 9 (all inclusve)
  yieldMany [3..9]
  -- then yields 30 to 35 (all inclusive)
  yieldMany [30..35]

-- Using Conduit as monad (example 2)
sinkConduitMonad :: Monad m => ConduitM Int o m (Text, Int, Product Int)
sinkConduitMonad = do
  -- take first 4 Ints, convert each to a Text, then use Monoid instance of
  -- Text to concatenate all of them
  s <- takeC 4 .| mapC (pack . show) .| foldC
  -- take the next 5 Ints and sum them
  sum5 <- takeC 5 .| sumC
  -- take the rest of the Ints, convert them to `Product Int`, then use Monoid
  -- instance of Product Int to sum them
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
  -- take first 4 values and multiply them by 2
  takeC 4 .| mapC (*2)
  -- add 7 to the rest of the values
  mapC (+7)

-- Using Conduit as monad - first 3, next 3, final 3, discard (exercise)
interConduitMFirst3Next3Last3 :: Monad m => ConduitM Int Int m ()
interConduitMFirst3Next3Last3 = do
  -- take first 3 values and subtract 1 from them
  takeC 3 .| mapC (\x -> x - 1)
  -- take next 3 values and add 1 to them
  takeC 3 .| mapC (+1)
  -- take next 3 values and multiple them by 3
  takeC 3 .| mapC (*3)
  -- discard the remaining values
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
