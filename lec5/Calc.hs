-- add Parser.hs StackVM.hs
import Parser
import StackVM



eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y




evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul