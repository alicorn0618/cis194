fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . map (+ 1) . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l y r)
    | height l < height r = Node h (insert x l) y r
    | otherwise = Node h l y (insert x r)
    where height Leaf = -1
          height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf


xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ filter (\x ->  x `notElem` map (\ (i, j) -> i + j + 2 * i * j) (cartProd [1 .. n] [1 .. n])) [1 .. n]
