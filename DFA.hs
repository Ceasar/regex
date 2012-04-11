{-# OPTIONS_GHC -Wall #-}

{- Deterministic Finite Automaton
 - CIS 194
 - Ceasar Bautista, Adi Dahiya
 -}


module DFA where

{- DFA M is defined by
 -- finite set of states (Q)
 -- finite set of input symbols called the alphabet (A)
 -- transition function (D)
 -- start state (Q)
 -- set of accept states (F)
 -}
type State = Integer

data DFA a b = DFA [a] [b] (a -> b -> a) a [a]

accepts :: Eq a => DFA a b -> [b] -> Bool
accepts (DFA _ _ _ q fs) [] = q `elem` fs
accepts (DFA qs a d q fs) (x:xs) = accepts (DFA qs a d (d q x) fs) xs

{-
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

union :: DFA -> DFA -> DFA
union (DFA qs a d q fs) (DFA rs b e r gs) = (DFA 
-}


-- Test functions

testTransition :: State -> Char -> State
testTransition 1 'a' = 1
testTransition 1 'b' = 2
testTransition 2 'a' = 2
testTransition 2 'b' = 1

testDFA :: DFA State Char
testDFA = DFA [1, 2] "ab" testTransition 1 [2]
