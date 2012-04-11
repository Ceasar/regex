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

data DFA a b = DFA {states :: [a],
                    alphabet :: [b],
                    transition :: (a -> b -> a),
                    start :: a,
                    accept :: [a]}

run :: (a -> b -> a) -> a -> [b] -> [a]
run _ _ [] = []
run f e (x:xs) = q : run f q xs where q = f e x

runDFA :: DFA a b -> [b] -> [a]
runDFA dfa xs = run (transition dfa) (start dfa) xs

accepts :: Eq a => DFA a b -> [b] -> Bool
accepts dfa xs = last (runDFA dfa xs) `elem` (accept dfa)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

union :: Eq a => Eq c => DFA a b -> DFA c b -> DFA (a, c) b
union (DFA qs a d q fs) (DFA rs _ e r gs) = (DFA xs z f x hs)
    where
        xs = (cartesianProduct qs rs)
        z = a
        f = (\s c -> (d (fst s) c, e (snd s) c))
        x = (q, r)
        hs = filter (\p -> (fst p) `elem` fs || (snd p) `elem` gs) (cartesianProduct qs rs)

intersection :: Eq a => Eq c => DFA a b -> DFA c b -> DFA (a, c) b
intersection (DFA qs a d q fs) (DFA rs _ e r gs) = (DFA xs z f x hs)
    where
        xs = (cartesianProduct qs rs)
        z = a
        f = (\s c -> (d (fst s) c, e (snd s) c))
        x = (q, r)
        hs = filter (\p -> (fst p) `elem` fs && (snd p) `elem` gs) (cartesianProduct qs rs)


-- Test functions
type State = Integer

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
