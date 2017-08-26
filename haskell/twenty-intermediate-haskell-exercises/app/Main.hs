module Main where


import Lib

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
instance Fluffy [] where
  furry = map

-- Exercise 2
instance Fluffy Maybe where
  furry = fmap

-- Exercise 3
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft . Left $ f x

-- Exercise 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight r) = EitherRight $ fmap f r

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  furry' :: (a -> b) -> m a -> m b
  furry' f x = (unicorn . f) `banana` x

-- Exercise 7
instance Misty [] where
  banana = concatMap
  unicorn x = [x]

-- Exercise 8
instance Misty Maybe where
  banana = flip (>>=)
  unicorn = Just

-- Exercise 9
instance Misty ((->) t) where
  banana = flip (>>=)
  unicorn = const

-- Exercise 10
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  unicorn = EitherLeft . Left

-- Exercise 11
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  unicorn = EitherRight . Right

-- Exercise 12
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = (\f -> furry' f ma) `banana` mf

-- Exericse 14
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy l f = foldr (\x acc -> apple acc (furry' (:) (f x))) (unicorn []) l

-- Exercise 15
sausage :: (Misty m) => [m a] -> m [a]
sausage = (flip moppy) id

-- Exercise 16
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = mb `apple` furry' f ma

-- Exercise 17
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = mc `apple` banana2 f ma mb

-- Exercise 18
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = md `apple` banana3 f ma mb mc

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
instance Fluffy (State s) where
  furry f st = State $ \s -> let (s', a) = state st s in (s', f a)

-- Exercise 20
instance Misty (State s) where
  banana f sa = let (s, a) = state sa s in f a
  unicorn a = State $ \s -> (s, a)

main :: IO ()
main = someFunc
