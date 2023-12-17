import Language.Haskell.TH (match)

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList [x] = [x]
doubleList (x : y : xs) = x : 2 * y : doubleList xs

doubleEveryOther :: Integer -> [Integer]
doubleEveryOther n = reverse (doubleList (toDigitsRev n))

sumDigits :: [Integer] -> Integer
sumDigits lst = sum (map (sum . toDigits) lst)

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther n)) 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a