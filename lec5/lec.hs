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



data BookInfo = Book {
    bookId :: Int,
    bookTitle :: String,
    bookAuthors :: [String]
} deriving (Show)

data PersonInfo = Person {
    name :: String,
    sex :: String,
    idNum :: Int
} deriving (Show)

class BasicEq a where
    isEqual :: a -> a -> Bool
    isNotEqual :: a -> a -> Bool

instance BasicEq BookInfo where
    isEqual x y = bookId x == bookId y && bookTitle x == bookTitle y && bookAuthors x == bookAuthors y
    isNotEqual x y = not $ isEqual x y

instance BasicEq PersonInfo where
    isEqual x y = name x == name y && sex x == sex y && idNum x == idNum y
    isNotEqual x y = not $ isEqual x y


