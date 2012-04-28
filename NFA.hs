{-# OPTIONS_GHC -Wall #-}

{- Nondeterministic Finite Automaton
 - CIS 194
 - Ceasar Bautista, Adi Dahiya
 -}


module NFA where
import Control.Monad


cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]


data NFA a b = NFA { states     :: [a],
                     alphabet   :: [b],
                     transition :: (a -> b -> [a]),
                     start      :: a,
                     accept     :: [a] }


-- Takes advantage of foldM to generate a list of possible states given a string
accepts :: Eq a => NFA a b -> [b] -> Bool
accepts nfa = any (`elem` (accept nfa)) . foldM (transition nfa) (start nfa)


concatenate :: Eq a => Eq b => NFA a b -> NFA a b -> NFA a b
concatenate (NFA qs a d q fs) (NFA rs _ e r gs) = NFA xs z f x hs
    where
        xs = qs ++ rs
        z = a
        f = \s c -> if (s `elem` gs) then e s c
                    else if (s `elem` fs) 
                        then if (c `elem` a)
                            then d s c
                            else r : d s c      -- epsilon transition
                        else d s c
        x  = q
        hs = gs


kleene :: Eq a => NFA a b -> NFA a b
kleene (NFA qs a d q fs) = undefined


-- Test NFA
-- Empty string should be represented by _
f1 :: Int -> Char -> [Int]
f1 1 'a' = [1]
f1 1 'b' = [1, 2]
f1 2 'a' = [1]
f1 2 'b' = [1]
f1 _ _   = []

nfa1 :: NFA Int Char
nfa1 = NFA [1, 2] "ab" f1 1 [2]

f2 :: Int -> Char -> [Int]
f2 1 'a' = [2]
f2 2 'a' = [1]
f2 _ _   = []

nfa2 :: NFA Int Char
nfa2 = NFA [1, 2] "a" f2 1 [2]

