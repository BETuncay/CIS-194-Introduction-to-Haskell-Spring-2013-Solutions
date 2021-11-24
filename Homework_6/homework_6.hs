import Data.List

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = map fib [1..]

--Exercise 2
fib' :: Integer -> Integer
fib' =  genericIndex fibs'
  where fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')


--Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
    show a = show (take 20 (streamToList a))


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))


-- Exercise 4
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Summary: Always interleve by n, with n = [0..]
-- Input  1 2 3 4 5 6 7 8 9 10
-- Output 0 1 0 2 0 1 0 3 0 1
-- (interleaved with 0) 
-- Lets omit the 0 cases
-- Input  2 4 6 8 10 12 14 16 18
-- Output 1 2 1 3 1  2  1  3  1
-- (interleaved with 1) 
-- Lets omit the 1 cases
-- Input  4 8 12 16 20 24 28 32
-- Output 2 3 2  3  2  3  2  5
-- (interleaved with 1) 
-- This pattern continues
-- To reproduce this pattern in a stream:
-- -> The order of interleving is descending -> egg ..., 4, 3, 2, 1, 0
-- However we cant execute in that order since we would need to start with infinite

-- interleave element a in input stream with input space
interleaveStreams :: a -> Integer -> Integer -> Stream a -> Stream a
interleaveStreams x acc step (Cons a b)
    | acc < step = Cons a (interleaveStreams x (acc + 1) step b)
    | acc == step = Cons x (interleaveStreams x 1 step b)
    | otherwise = streamRepeat a -- errorcase

applyStream :: Integer -> Stream Integer -> Stream Integer
applyStream x stream = applyStream (x+1) (interleaveStreams x 1 (2^x) stream)

-- should be correct but cant be evalutated due to execution order
ruler :: Stream Integer
ruler = applyStream 0 (streamRepeat 0)
