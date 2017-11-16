#!/usr/bin/env stack
-- stack script --resolver=lts-9.4


{--

About
=====
Extra work we had to do to get the example in the link below to work:

    https://wiki.haskell.org/Arrow_tutorial#Kleisli_Arrows

That said, we are still quite clueless about arrows.


References
==========
- https://stackoverflow.com/questions/18925045/why-there-isnt-a-functor-instance-for-kleisli-in-control-arrow


TODO
====
Prove that the functor and applicative instances of (Kleisli m a) are correct
--}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>), Kleisli(Kleisli), arr, runKleisli)

instance (Monad m) => Functor (Kleisli m a) where
  fmap f a = a >>> arr f

instance (Monad m) => Applicative (Kleisli m a) where
  pure x = arr $ const x
  af <*> av = Kleisli $ \a -> do
    f <- (runKleisli af) a
    v <- (runKleisli av) a
    return $ f v

plusminus, double, h2 :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])
double = arr (* 2)
h2 = liftA2 (+) plusminus double

main :: IO ()
main = putStrLn . show $ runKleisli h2 8
