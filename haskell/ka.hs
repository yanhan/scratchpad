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

instance Functor (Kleisli [] Int) where
  fmap f k = Kleisli (\y ->
    let l = runKleisli k y
    in fmap f l)

{--
Proof that (Kleisli [] Int) is a functor

fmap f (fmap g x)
= fmap f $ Kleisli (\y ->
    let l = runKleisli x y
    in fmap g l)
= Kleisli (\y ->
    let l = runKleisli $ Kleisli (\y ->
      let l = runKleisli x y
      in fmap g l) y
    in fmap f l)
= Kleisli (\y ->
    let l = fmap g (runKleisli x y)
    in fmap f l)
= Kleisli (\y -> fmap f (fmap g (runKleisli x y)))
= Kleisli (\y -> fmap (f . g) (runKleisli x y)) -- since (runKleisli x y) is a list, so we can compose the fmaps

fmap (f . g) x
= Kleisli (\y ->
    let l = runKleisli x y
    in fmap (f . g) l)
--}

instance Applicative (Kleisli [] Int) where
  pure x = Kleisli (\_ -> [x])
  af <*> ax = Kleisli (\y ->
    let lf = runKleisli af y
        lx = runKleisli ax y
    in lf <*> lx)

{--
Proof that (Kleisli [] Int) is an applicative

Law 1: pure id <*> v = v

pure id <*> v
= Kleisli (\_ -> [id]) <*> v
= Kleisli (\y ->
    let lf = runKleisli (Kleisli (\_ -> [id])) y
        lx = runKleisli v y
    in lf <*> lx)
= Kleisli (\y ->
    let lf = [id]
        lx = runKleisli v y
    in lf <*> lx)
= Kleisli (\y -> runKleisli v y)
= v


Law 2: pure f <*> pure x = pure (f x)

pure f <*> pure x
= Kleisli (\_ -> [f]) <*> Kleisli (\_ -> [x])
= Kleisli (\y ->
    let lf = runKleisli (Kleisli (\_ -> [f])) y
        lx = runKleisli (Kleisli (\_ -> [x])) y
    in lf <*> lx)
= Kleisli (\y ->
    let lf = [f]
        lx = [x]
    in lf <*> lx)
= Kleisli (\y -> [f x])
= Kleisli (\_ -> [f x])
= pure (f x)


Law 3: u <*> pure y = pure ($ y) <*> u

u <*> pure y
=  u <*> Kleisli (\_ -> [y])
= Kleisli (\x ->
    let lf = runKleisli u x
        lx = runKleisli (Kleisli (\_ -> [y])) x
    in lf <*> lx)
= Kleisli (\x ->
    let lf = runKleisli u x
        lx = [y]
    in lf <*> lx)

pure ($ y) <*> u
= Kleisli (\_ -> [($ y)]) <*> u
= Kleisli (\x ->
    let lf = runKleisli (Kleisli (\_ -> [($ y)])) x
        lx = runKleisli u x
    in lf <*> lx)
= Kleisli (\x ->
    let lf = [($ y)]
        lx = runKleisli u x
    in lf <*> lx)

which are equivalent


Law 4: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

pure (.) <*> u <*> v <*> w
= Kleisli (\_ -> [(.)]) <*> u <*> v <*> w
= Kleisli (\y ->
    let lf = runKleisli Kleisli (\_ -> [(.)]) y
        lx = runKleisli u y
    in lf <*> lx) <*> v <*> w
= Kleisli (\y ->
    let lf = [(.)]
        lx = runKleisli u y
    in lf <*> lx) <*> v <*> w
= Kleisli (\z ->
    let lf = runKleisli (Kleisli (\y ->
               let lf = [(.)]
                   lx = runKleisli u y)) z
        lx = runKleisli v z
    in lf <*> lx) <*> w
= Kleisli (\z ->
    let lf = [(.)] <*> runKleisli u z
        lx = runKleisli v z
    in lf <*> lx) <*> w
= Kleisli (\a ->
    let lf = runKleisli (Kleisli (\z ->
               let lf = [(.)] <*> runKleisli u z
                   lx = runKleisli v z
               in lf <*> lx)) a
        lx = runKleisli w a
    in lf <*> lx)
= Kleisli (\a ->
    let lf = (let lf = [(.)] <*> runKleisli u a
                  lx = runKleisli v a
               in lf <*> lx)
        lx = runKleisli w a
    in lf <*> lx)
= Kleisli (\a ->
    let lf = [(.)] <*> runKleisli u a <*> runKleisli v a
        lx = runKleisli w a
    in lf <*> lx)
= Kleisli (\a -> [(.)] <*> runKleisli u a <*> runKleisli v a <*> runKleisli w a)
= Kleisli (\a ->
    let lf = [f . g | f <- runKleisli u a, g <- runKleisli v a]
        lx = runKleisli w a
    in lf <*> lx)
= Kleisli (\a ->
    let lf = [f . g | f <- runKleisli u a, g <- runKleisli v a]
        lx = runKleisli w a
    in [f x | f <- lf, x <- lx])
= Kleisli (\a ->
  in [(f . g) x | f <- runKleisli u a, g <- runKleisli v a, x <- runKleisli w a])

u <*> (v <*> w)
= Kleisli (\x ->
    let lf = runKleisli u x
        lx = runKleisli (v <*> w) x
    in lf <*> lx)
= Kleisli (\x ->
    let lf = runKleisli u x
        lx = runKleisli (Kleisli \a ->
               let lf = runKleisli v a
                   lx = runKleisli w a
               in lf <*> lx) x
    in lf <*> lx)
= Kleisli (\x ->
    let lf = runKleisli u x
        lx = runKleisli v x <*> runKleisli w x
    in lf <*> lx)
= Kleisli (\a ->
    let lf = runKleisli u a
        lx = [g x | g <- runKleisli v a, runKleisli w a]
    in lf <*> lx)
= Kleisli (\a ->
    let lf = runKleisli u a
        lx = [g x | g <- runKleisli v a, runKleisli w a]
    in [f y | f <- lf, y <- lx])
= Kleisli (\a ->
    [f y | f <- runKleisli u a, y <- [g x | g <- runKleisli v a, runKleisli w a]])
= Kleisli (\a ->
    [f (g x) | f <- runKleisli u a, g <- runKleisli v a, x <- runKleisli w a]])

which are the same
--}

h2Output :: [Int]
h2Output = runKleisli h2 8

main :: IO ()
main = putStrLn . show $ h2Output
