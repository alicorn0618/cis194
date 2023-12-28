data IntList = Empty | Cons Int IntList
    deriving Show

-- perfom some operation on every element of the list
-- What sorts of things might we want to do with an IntList? Here are a few common possibilities:
-- Perform some operation on every element of the list
-- Keep only some elements of the list, and thorw others away, based on a test
-- "Summarize" the list in some way, (find the sum, find the product, find the maximum element, etc.)
-- You can probably think of others

-- Map

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)


exapmleList = Cons (-1) (Cons 2 (Cons (-6) Empty))


addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

-- mapIntList addOne exapmleList
-- mapIntList square exapmleList
-- mapIntList abs exapmleList

_ = mapIntList addOne exapmleList
_ = mapIntList square exapmleList
_ = mapIntList abs exapmleList


-- Filter

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs


data List t = E | C t (List t)
    deriving Show

