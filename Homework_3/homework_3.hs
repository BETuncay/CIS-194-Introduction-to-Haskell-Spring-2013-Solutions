{-# OPTIONS_GHC -Wall #-}
module Golf where

--import Data.List
-- Exercise 1
getNth :: Int -> [a] -> [a]
getNth num list = map snd (filter (\(x,_) -> mod x num == 0) (zip [1..(length list)] list))

skips :: [a] -> [[a]]
skips list = map (\(x,_) -> getNth x list) (zip [1..(length list)] list)


-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (left:mid:right:rest) = if mid > left && mid > right
    then mid : localMaxima (mid:right:rest)
    else localMaxima (mid:right:rest)
localMaxima _ = []


-- Exercise 3
countElement :: [Integer] -> Integer -> Integer
countElement list element = sum (map (\x -> if x == element then 1 else 0) list)

countAllElement :: [Integer] -> [Integer]
countAllElement list = map (countElement list) [1..9]

intToStar :: Integer -> Char
intToStar 0 = ' '
intToStar _ = '*'

decrease :: [Integer] -> [Integer] -> [Integer] 
decrease [] _ = []
decrease (x:xs) rmList = if x `elem` rmList 
    then x : decrease xs rmList 
    else decrease xs (x:rmList)

histogram :: [Integer] -> String
histogram [] ="==========\n0123456789\n"
histogram list = let amount = countAllElement list
    in map intToStar amount ++ ['\n'] ++ histogram (decrease list [])