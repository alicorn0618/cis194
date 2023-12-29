-- f :: a -> a -> a
-- f x y = x && y

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

instance Listable [Int] where
    toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)



instance Listable (Tree Int) where
    toList Empty = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum $ toList x

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y


instance (Listable a, Listable b) => Listable (a, b) where
    toList (x, y) = toList x ++ toList y