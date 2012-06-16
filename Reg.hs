module Reg where


import Data.Set as S

import NFA


type InfixExpr = String
type PostfixExpr = String
data Reg = Epsilon
         | Literal Char
         | Or Reg Reg
         | Concat Reg Reg
         | Star Reg
    deriving Eq


-- | Display a human-readable regular expression.
showReg :: Reg -> [Char]
showReg Epsilon        = "@"
showReg (Literal c)    = [c]
showReg (Or r1 r2)     = "(" ++ showReg r1 ++  "|" ++ showReg r2 ++ ")"
showReg (Concat r1 r2) = "(" ++ showReg r1 ++ showReg r2 ++ ")"
showReg (Star r)       = showReg r ++ "*"


instance Show Reg where
    show = showReg


-- | Convert a postfix regex expression into a parse tree
evalPostfix :: PostfixExpr -> Reg
evalPostfix = head . Prelude.foldl comb []
    where
        comb :: [Reg] -> Char -> [Reg]
        comb (x:y:ys) '|'   = (Or y x) : ys
        comb (x:y:ys) '&'   = (Concat y x) : ys
        comb (x:xs) '*'     = (Star x) : xs
        comb xs '@'         = Epsilon : xs
        comb xs s           = (Literal s) : xs


-- | Apply the shunting-yard algorithm to turn an infix expression
-- | into a postfix expression.
-- | Regexs must use implicit concatenations
shunt :: [Char] -> [Char] -> InfixExpr -> PostfixExpr
shunt o p [] = (reverse o) ++ p
shunt o [] (x:xs)
    | x == '(' = shunt o [x] xs
    | x == '|' = shunt o [x] xs
    | x == '&' = shunt o [x] xs
    | x == '*' = shunt (x:o) [] xs
    | otherwise = shunt (x:o) [] xs
shunt o (p:ps) (x:xs)
    | x == '(' = shunt o (x:p:ps) xs
    | x == ')' = case (span (/= '(') (p:ps)) of
        (as, b:bs) -> shunt (as ++ o) bs xs
    | x == '|' = case (p) of
        '(' -> shunt o (x:p:ps) xs
        otherwise -> shunt (p:o) (x:ps) xs
    | x == '&' = shunt (o) (x:p:ps) xs
    | x == '*' = shunt (x:o) (p:ps) xs
    | otherwise = shunt (x:o) (p:ps) xs


-- | Convert an infix expression to postfix
toPostfix :: InfixExpr -> PostfixExpr
toPostfix = shunt [] []


-- | Evaluate an infix expression
eval :: InfixExpr -> Reg
eval = evalPostfix . toPostfix


-- | Build a NFA from a regular expression
build :: Reg -> Nfa Int
build (Literal c)    = NFA (fromList [0, 1]) (singleton (Move 0 c 1)) 0 (singleton 1)
build (Or r1 r2)     = nfaUnion (build r1) (build r2)
build (Concat r1 r2) = nfaConcat (build r1) (build r2)
build (Star r)       = nfaStar (build r)


-- | Check if a regex matches a word
matches :: String -> String -> Bool
matches s = accepts (build (eval s))
