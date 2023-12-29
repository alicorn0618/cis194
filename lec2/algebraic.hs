data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

-- a new type called Thing with  five data constructors
-- Show is a magical incantation which tells GHC to automatically generate code
-- to convert Thing values to Strings

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True

-- Beyond enumerations
-- Thing is an enumeration type, similar to those

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

-- This says that FailableDouble type has two data constructors
-- The first one, Failure, takes no arguments, so Failure by itself is a value
-- of type FailableDouble
-- The second one. OK, takes an argument of type Double. So OK by itself is not
-- a value of type FailableDouble, but OK 3.4 is

ex01 = Failure

ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- More pattern-matching! Notice how in the OK case we can give a name to the
-- value inside the OK constructor. This is called binding a name to a value.

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- Notice how the Person constructor takes three arguments, but we only pattern
-- match on one of them. The other two are discarded using the _ pattern.

-- Algebraic data types in general
-- In general, an algebraic data type has one or more data constructors, and
-- each data constructor can have zero or more arguments. Each argument must be

-- data AlgDataType = Constr1 Type11 Type12
--                  | Constr2 Type21
--                  | Constr3 Type31 Type32 Type33
--                  | Constr4

-- The type AlgDataType is called the type's *name* or *type constructor*
-- The parts after the equals sign are called the type's *data constructors*
-- The parts after the data constructors are called the type's *value constructors*

-- One final note: the type of a data constructor names must always with a capital
-- letter; variables and type variables must always start with a lowercase letter.

-- Pattern-matching
-- Fundamentally, pattern-matching is about taking apart a value by finding out
-- which constructor it was built with, and which values were used as arguments
-- the information can be used as the basis for deciding what to do--indeed, in
-- Haskell, this is the only way to make a decision.

-- For example, to decide what to do with a value of type AlgDataType (the made-up

-- foo (Constr1 a b) = ...
-- foo (Constr2 a)  = ...
-- foo (Constr3 a b c) = ...
-- foo Constr4 = ...

-- Note how we also get to give names to the values that come along with each constructor
-- Note also that parentheses are required around patterns consisting of just a single constructor

-- This is the main idea behind patterns, but there are a few things to note.
-- 1. An underscore _ can be used as a "wildcard pattern" which matches anything
-- 2. A pattern of the form x@pat can be used to match a value against the pattern pat,
-- also give the name x to the entire value being matched. For example:
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- 3. Patterns can be nested. For example
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

-- In general, the following grammar defines with can be used a pattern:
-- pat ::= _
--       | var
--       | var @ ( pat )
--       | ( Constructor pat1 pat2 ... patn)

-- Case expressions

-- case exp of
--  pat1 -> exp1
--  pat2 -> exp2
--  ...

ex03 = case "Hello" of
  [] -> 3
  ('H' : s) -> length s
  _ -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d -> d

-- Recursive data types
data IntList = IEmpty | Cons Int IntList

intListProd :: IntList -> Int
intListProd IEmpty = 1
intListProd (Cons x l) = x * intListProd l

data Tree
  = Leaf Char
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))