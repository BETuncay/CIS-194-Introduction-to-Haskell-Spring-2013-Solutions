-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | x < 10 = [x]
  | otherwise = toDigits (div (x - mod x 10) 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | null xs = xs
  | even (length xs) = head xs * 2 : doubleEveryOther (tail xs)
  | otherwise = head xs : doubleEveryOther (tail xs)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (toDigits (head xs)) + sumDigits (tail xs)

-- Exercise 4
validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 2 a b c = [(a,c), (a,b), (c,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
