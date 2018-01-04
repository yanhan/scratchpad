#!/usr/bin/env stack
-- stack script --resolver lts-9.4

-- From: https://wiki.haskell.org/Arrow_tutorial#Kleisli_Arrows

{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative (liftA2)
import Control.Arrow (Kleisli(Kleisli, runKleisli), arr)

plusminus :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])

double :: Kleisli [] Int Int
double = arr (* 2)

h2 :: Kleisli [] Int Int
h2 = liftA2 (+) plusminus double

instance Applicative (Kleisli [] Int) where
  pure x = Kleisli (\_ -> [x])
  af <*> ax = Kleisli (\y ->
    let lf = runKleisli af y
        lx = runKleisli ax y
    in lf <*> lx)

instance Functor (Kleisli [] Int) where
  fmap f k = Kleisli (\y ->
    let l = runKleisli k y
    in fmap f l)

h2Output :: [Int]
h2Output = runKleisli h2 8

main :: IO ()
main = putStrLn . show $ h2Output
