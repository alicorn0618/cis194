data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Int
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Int -> Int
treeSum' = treeFold 0 (\l x r -> x + l + r)

treeDepth' :: Tree a -> Int
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> max x (max l r))

data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit x) = f x
exprTFold f g h (Add x y) = g (exprTFold f g h x) (exprTFold f g h y)
exprTFold f g h (Mul x y) = h (exprTFold f g h x) (exprTFold f g h y)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)

newtype Sum a = Sum a
    deriving (Eq, Ord, Num, Show)

