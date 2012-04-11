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

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

union :: DFA a b -> DFA c b -> DFA (a, c) b
union (DFA qs a d q fs) (DFA rs _ e r gs) = (DFA (cartesianProduct qs rs) a (\x c -> (d (fst x) c, e (snd x) c)) (q, r) ((cartesianProduct fs rs) ++ (cartesianProduct qs gs)))


-- Test functions

testTransition :: State -> Char -> State
testTransition 1 'a' = 1
testTransition 1 'b' = 2
testTransition 2 'a' = 2
testTransition 2 'b' = 1

testDFA :: DFA State Char
testDFA = DFA [1, 2] "ab" testTransition 1 [2]

testTransition2 :: State -> Char -> State
testTransition2 3 'a' = 3
testTransition2 3 'b' = 4
testTransition2 4 'a' = 3
testTransition2 4 'b' = 3

testDFA2 :: DFA State Char
testDFA2 = DFA [3, 4] "ab" testTransition2 3 [4]
