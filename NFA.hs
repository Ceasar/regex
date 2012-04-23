{-# OPTIONS_GHC -Wall #-}

{- Nondeterministic Finite Automaton
 - CIS 194
 - Ceasar Bautista, Adi Dahiya
 -}


module NFA where


cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]


data NFA a b = NFA { states     :: [a],
                     alphabet   :: [b],
                     transition :: (a -> b -> [a]),
                     start      :: a,
                     accept     :: [a] }


run :: (a -> b -> [a]) -> [b] -> a -> [a]
run _ [] q = [q]
run t (x:xs) q = foldr (++) [] (map (run t xs) (t q x))

accepts :: Eq a => NFA a b -> [b] -> Bool
accepts nfa w = any (`elem` (accept nfa)) (run (transition nfa) w (start nfa))


--Test NFA
-- Empty string should be represented by _
testTransition :: Int -> Char -> [Int]
testTransition 1 'a' = [1]
testTransition 1 'b' = [1, 2]
testTransition 2 'a' = [1]
testTransition 2 'b' = [1]
testTransition _ _ = []

testNFA :: NFA Int Char
testNFA = NFA [1, 2] "ab" testTransition 1 [2]
