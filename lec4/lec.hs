greaterThan100 :: [Integer] -> [Integer]
greaterThan100  = filter (> 100)


curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest2 :: [Integer] -> Bool
myTest2 = even . length . greaterThan100

-- currying and partial application
f :: Int -> Int -> Int
f x y = 2 * x + y

f' :: Int -> (Int -> Int)
f' x y = 2 * x + y


foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (> 3)

