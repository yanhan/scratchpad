#!/usr/bin/env stack
-- stack script --resolver lts-9.4

{-# LANGUAGE GADTs #-}

{--
data Expr = I Int
        | B Bool
        | Add Expr Expr
        | Mul Expr Expr
        | Eq Expr Expr

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)
eval (Add e1 e2) = do
  r1 <- eval e1
  r2 <- eval e2
  case (r1, r2) of
    (Left x, Left y) ->
      Just $ Left (x + y)
    _ -> Nothing
eval (Eq e1 e2) = do
  r1 <- eval e1
  r2 <- eval e2
  case (r1, r2) of
    (Left x, Left y) ->
      Just $ Right (x == y)
    _ -> Nothing
--}


{--
-- An improvement using Phantom Types
data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq (Expr a) (Expr a)

add :: Expr Int -> Expr Int -> Expr Int
add = Add

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B
--}


data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2

main :: IO ()
main = putStrLn . show $ eval (Add (I 5) (Mul (I 7) (B True)))
