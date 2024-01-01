import Data.Array

veryBigList = [1..1000000]

--
-- foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
-- foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

-- Strict Evaluation
-- function arugments are completely evaluated before the function is applied

-- Pattern Matching drives evaluation

f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing = []
f2 (Just x) = [x]

-- Expressions are only evaluated when pattern matched
-- only as far as necessary for the match to proceed and no farther

-- Short Circuiting

(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False