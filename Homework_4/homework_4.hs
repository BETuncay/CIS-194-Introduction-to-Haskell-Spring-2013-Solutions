{-# OPTIONS_GHC -Wall #-}

-- Exercise 1 
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x-2) * y) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2Helper :: Integer -> Integer
fun2Helper n
  | n == 1 = 0
  | even n = div n 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' x = sum $ filter even $ takeWhile (/=0) $ iterate fun2Helper x

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Cons a Nil -> Node 0 Leaf a Leaf
-- Cons a (Cons b Nil) -> Node 1 (Node 0 Leaf b Leaf) a Leaf
-- Cons a (Cons b (Cons c Nil)) -> Node 1 (Node 0 Leaf b Leaf) a (Node 0 Leaf c Leaf)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) =
  let
    height = floor (logBase 2 (fromIntegral (length (x:xs))))
    half = splitAt (div (length xs) 2) xs
    left = foldTree (fst half)
    right = foldTree (snd half)
  in Node height left x right


-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let primes = [x | x <- [2 .. 2 * n + 2]]
  in foldr (\a b -> a: filter (\x -> mod x a /= 0) b ) [] primes

-- Cons a (Cons b (Cons c ...)) -> (\x y -> Cons x Filter P y) a (Cons b (Cons c ...))