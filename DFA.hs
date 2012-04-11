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

data DFA = DFA [State] [Char] (State -> Char -> State) State [State]

run :: DFA -> String -> Bool
run (DFA _ _ _ q fs) [] = q `elem` fs
run (DFA qs a d q fs) (x:xs) = run (DFA qs a d (d q x) fs) xs
