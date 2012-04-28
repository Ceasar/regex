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

data DFA a b = DFA { states     :: [a],
                     alphabet   :: [b],
                     transition :: (a -> b -> a),
                     start      :: a,
                     accept     :: [a] }


run :: (a -> b -> a) -> a -> [b] -> [a]
run _ _ [] = []
run t q (o:v) = t q o : run t q v


runDFA :: DFA a b -> [b] -> [a]
runDFA dfa xs = run (transition dfa) (start dfa) xs


accepts :: Eq a => DFA a b -> [b] -> Bool
accepts dfa xs = foldl (transition dfa) (start dfa) xs `elem` (accept dfa)


cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]


union :: Eq a => Eq c => DFA a b -> DFA c b -> DFA (a, c) b
union (DFA qs a d q fs) (DFA rs _ e r gs) = DFA xs z f x hs
    where
        xs = cartesianProduct qs rs
        z  = a
        f  = \s c -> (d (fst s) c, e (snd s) c)
        x  = (q, r)
        hs = filter (\p -> (fst p) `elem` fs || (snd p) `elem` gs) (cartesianProduct qs rs)


intersection :: Eq a => Eq c => DFA a b -> DFA c b -> DFA (a, c) b
intersection (DFA qs a d q fs) (DFA rs _ e r gs) = DFA xs z f x hs
    where
        xs = cartesianProduct qs rs
        z  = a
        f  = \s c -> (d (fst s) c, e (snd s) c)
        x  = (q, r)
        hs = filter (\p -> (fst p) `elem` fs && (snd p) `elem` gs) (cartesianProduct qs rs)


concatenate :: DFA a b -> DFA a b -> DFA a b
concatenate (DFA qs a d q fs) (DFA rs _ e r gs) = DFA xs z f x hs
    where
        xs = cartesianProduct qs rs
        z  = a
        f  = \s c -> undefined
        x  = q
        hs = gs


kleene :: DFA a b -> DFA a b
kleene (DFA qs a d q fs) = undefined


-- Test functions
testTransition :: Int -> Char -> Int
testTransition 1 'a' = 1
testTransition 1 'b' = 2
testTransition 2 'a' = 2
testTransition 2 'b' = 1
testTransition _ _   = 1

testDFA :: DFA Int Char
testDFA = DFA [1, 2] "ab" testTransition 1 [2]

testTransition2 :: Int -> Char -> Int
testTransition2 3 'a' = 3
testTransition2 3 'b' = 4
testTransition2 4 'a' = 3
testTransition2 4 'b' = 3
testTransition2 _ _   = 3

testDFA2 :: DFA Int Char
testDFA2 = DFA [3, 4] "ab" testTransition2 3 [4]

