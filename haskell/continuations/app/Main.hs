module Main where

import Control.Monad (when)
import Control.Monad.Cont (Cont, callCC, runCont)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Char (digitToInt, intToDigit)
import Data.Monoid ((<>))

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
  square_cps x $ \x_squared ->
  square_cps y $ \y_squared ->
  add_cps x_squared y_squared $ k

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
  f_cps x $ \fx ->
  f_cps fx $ \ffx ->
  f_cps ffx $ k

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS f g = \k ->
  f $ \x -> g x k

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return $ add x y

square_cont :: Int -> Cont r Int
square_cont x = return $ square x

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
  x_squared <- square_cont x
  y_squared <- square_cont y
  add_cont x_squared y_squared

--pythagoras_cont x y =
--  square_cont x >>= \x_squared ->
--    square_cont y >>= \y_squared ->
--      add_cont x_squared y_squared

--pythagoras_cont x y =
--  ContT $ \c -> runContT (square_cont x) $ \x ->
--    runContT
--      ((\x_squared ->
--        square_cont y >>= \y_squared -> add_cont x_squared y_squared) x)
--      c

--pythagoras_cont x y =
--  ContT $ \c -> runContT (ContT ($ x * x)) $ \x ->
--    runContT
--      ((\x_squared ->
--        square_cont y >>= \y_squared -> add_cont x_squared y_squared) x)
--      c

--pythagoras_cont x y =
--  ContT $ \c -> ($ x * x) $ \x ->
--    runContT
--      ((\x_squared ->
--        square_cont y >>= \y_squared -> add_cont x_squared y_squared) x)
--      c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT
--      ((\x_squared ->
--        square_cont y >>= \y_squared -> add_cont x_squared y_squared) (x * x))
--      c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT
--      (square_cont y >>= \y_squared -> add_cont (x * x) y_squared)
--      c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT
--      (ContT $ \c ->
--        runContT
--          (square_cont y)
--          (\z -> runContT ((\y_squared -> add_cont (x * x) y_squared) z) c))
--      c

--pythagoras_cont x y =
--  ContT $ \c ->
--    (\c ->
--        runContT
--          (square_cont y)
--          (\z -> runContT ((\y_squared -> add_cont (x * x) y_squared) z) c))
--    c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT
--      (square_cont y)
--      (\z -> runContT ((\y_squared -> add_cont (x * x) y_squared) z) c)

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT
--      (ContT ($ y * y))
--      (\z -> runContT ((\y_squared -> add_cont (x * x) y_squared) z) c)

--pythagoras_cont x y =
--  ContT $ \c ->
--    ($ y * y)
--    (\z -> runContT ((\y_squared -> add_cont (x * x) y_squared) z) c)

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT ((\y_squared -> add_cont (x * x) y_squared) (y * y)) c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT (add_cont (x * x) (y * y)) c

--pythagoras_cont x y =
--  ContT $ \c ->
--    runContT (ContT ($ x * x + y * y)) c

--pythagoras_cont x y = ContT $ \c -> ($ x * x + y * y) c


squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)

--fooTwo :: Int -> Cont r String
--fooTwo x = callCC $ \k -> do
--  let y = x ^ 2 + 3
--  when (y > 20) $ k "over twenty"
--  return (show $ y - 4)

--fooTwo x = ContT $ \c ->
  --let f = \k -> do
        --let y = x ^ 2 + 3
        --when (y > 20) $ k "over twenty"
        --return (show $ y - 4)
  --in runContT (f (\x -> ContT $ \_ -> c x)) c

--fooTwo x = ContT $ \c ->
  --runContT (do
      --let y = x ^ 2 + 3
      --when (y > 20) $ ((\x -> ContT $ \_ -> c x) "over twenty")
      --return (show $ y - 4))
    --c

--fooTwo x = ContT $ \c ->
  --runContT (
      --let y = x ^ 2 + 3 in do
      --when (y > 20) $ ((\x -> ContT $ \_ -> c x) "over twenty")
      --return (show $ y - 4))
    --c

--fooTwo x = ContT $ \c ->
  --runContT (
      --let y = x ^ 2 + 3 in
        --when (y > 20) ((\x -> ContT $ \_ -> c x) "over twenty") >>
          --return (show $ y - 4))
    --c

--fooTwo x = ContT $ \c ->
  --runContT (
      --when ((x^2 + 3) > 20) ((\x -> ContT $ \_ -> c x) "over twenty") >>
        --return (show $ (x^2 + 3) - 4))
    --c

--fooTwo x = ContT $ \c ->
  --runContT (
    --(if (x^2 + 3) > 20
       --then ((\x -> ContT $ \_ -> c x) "over twenty")
       --else pure ()) >>
         --return (show $ (x^2 + 3) - 4))
    --c

--fooTwo x = ContT $ \c ->
  --runContT (
    --(if (x^2 + 3) > 20
       --then ((\x -> ContT $ \_ -> c x) "over twenty")
       --else pure ()) >>= \_ ->
         --return (show $ (x^2 + 3) - 4))
    --c


 --Assuming x = 5
--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
  --runContT (
     --((\x -> ContT $ \_ -> c x) "over twenty") >>= \_ ->
       --return (show $ (5^2 + 3) - 4))
    --c

--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
  --runContT (
     --(ContT $ \_ -> c "over twenty") >>= \_ ->
       --return (show $ (5^2 + 3) - 4))
    --c

--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
  --runContT (
    --ContT $ \c ->
      --runContT (ContT $ \_ -> c "over twenty") $ \x ->
        --runContT ((\_ -> return (show $ (5^2 + 3) - 4)) x) c)
    --c

--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
    --(\c ->
      --runContT (ContT $ \_ -> c "over twenty") $ \x ->
        --runContT ((\_ -> return (show $ (5^2 + 3) - 4)) x) c)
    --c

--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
  --runContT (ContT $ \_ -> c "over twenty") $ \x ->
    --runContT ((\_ -> return (show $ (5^2 + 3) - 4)) x) c

--fooTwo :: Cont r String
--fooTwo = ContT $ \c ->
  --(\_ -> c "over twenty") $ \x ->
    --runContT ((\_ -> return (show $ (5^2 + 3) - 4)) x) c

-- m >>= k = ContT $ \c -> runContT m (\x -> runContT (k x) c)
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
-- callCC f = ContT $ \c -> runContT (f (\x -> ContT $ \_ -> c x)) c
fooTwo :: Cont r String
fooTwo = ContT $ \c -> c "over twenty"

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c : s
    when (s0 == "hello") $ k "They say hello."
    let s1 = show s0
    return $ "They appear to be saying " <> s1
  return $ length msg

barTwo :: Char -> String -> Cont r Int
barTwo c s =
  let f = \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello."
        let s1 = show s0
        return $ "They appear to be saying " <> s1
  in (ContT $ \c -> runContT (f (\x -> ContT $ \_ -> c x)) c) >>= \msg -> ContT $ \c -> c (length msg)

fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do
    when (n < 10) (exit1 (show n))
    let ns = map digitToInt (show (n `div` 2))
    n' <- callCC $ \exit2 -> do
      when ((length ns) < 3) (exit2 (length ns))
      when ((length ns) < 5) (exit2 n)
      when ((length ns) < 7) $ do
        let ns' = map intToDigit (reverse ns)
        exit1 (dropWhile (== '0') ns')
      return $ sum ns
    return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
  return $ "Answer: " ++ str

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err

main :: IO ()
main = do
  runCont (bar 'h' "ello") print
  runCont (barTwo 'h' "ello") print
  runCont (bar 'c' "arpooling") print
  add_cps 3 6 print
  square_cps 4 print
  pythagoras_cps 7 9 print
  thrice_cps (add_cps 2) 5 print
  chainCPS (add_cps 3 4) (add_cps 5) print
  runCont (add_cont 1 3) print
  runCont (square_cont 9) print
  runCont (pythagoras_cont 3 4) print
  runCont (squareCCC 4) print
  runCont (foo 5) print
  runCont (foo 1) print
  runCont (bar 'h' "ello") print
  runCont (bar 'c' "arpooling") print
  print $ fun 5
  print $ fun 10
  print $ fun 200
  print $ fun 20000
  print $ fun 2000000
  let x = runCont (divExcpt 7 3 error) id
  print x
  let y = runCont (divExcpt 7 0 error) id
  print y
