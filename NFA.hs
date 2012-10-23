module NFA (Nfa(NFA), run, accepts, {-nfaUnion, nfaConcat, nfaStar-}) where


import Data.Set as S

type State = Int

data Nfa = NFA  (Set State) -- Set of states
                (Maybe Char -> State -> Set State) -- Relation
                State -- Starting state
                (Set State) -- Final states


-- | Keep iterating until a static set remains
setlimit :: (Set State -> Set State) -> Set State -> Set State
setlimit f s
    | s == next = s
    | otherwise = setlimit f next
      where
        next = f s


-- | Compute the set of states reachable from epsilon transitions
closure :: Nfa -> Set State -> Set State
closure (NFA _ f _ _) qs = setlimit (S.fold (S.union . f Nothing) qs) qs


-- | Compute the set of states reachable from symbolic transitions
onemove :: Nfa -> Char -> Set State -> Set State
onemove (NFA _ f _ _) c = S.fold (S.union . f (Just c) ) S.empty


-- | Compute the state of the machine after a single transition
onetrans ::  Nfa -> Set State -> Char -> Set State
onetrans m x c = closure m (onemove m c x)


-- | Compute the final state of an NFA on a string
run :: Nfa -> String -> Set State
run m@(NFA _ _ q _) = Prelude.foldl (onetrans m) (closure m (singleton q))


-- | Check if a NFA accepts a word
accepts :: Nfa -> String -> Bool
accepts m@(NFA _ _ _ fs) = not . S.null . S.intersection fs . run m


{-
-- | Combine two NFAs which recognize languages L1 and L2 to form a new
-- NFA which recognizes L1 | L2.
nfaUnion :: Nfa -> Nfa -> Nfa
nfaUnion (NFA states1 moves1 _ _) (NFA states2 moves2 _ _)
    = NFA (fromList [0..(m1+m2+1)])
          (moves1' `union` moves2' `union` newmoves)
          0
          (singleton (m1 + m2 + 1))
          where
            m1 = size states1
            m2 = size states2
            moves1'  = mapMonotonic (renumberMove 1)        moves1
            moves2'  = mapMonotonic (renumberMove (m1 + 1)) moves2
            newmoves = fromList [Emove 0 1,
                                Emove 0 (m1 + 1),
                                Emove m1 (m1 + m2 + 1),
                                Emove (m1 + m2) (m1 + m2 + 1)]


-- | Combine two NFAs which recognize languages L1 and L2 to form a new
-- NFA which recognizes L1 + L2.
nfaConcat :: Nfa -> Nfa -> Nfa
nfaConcat (NFA states1 moves1 _ _) (NFA states2 moves2 _ _)
    = NFA (fromList [0..(m1 + m2 - 1)])
          (moves1' `union` moves2' `union` newmoves)
          0
          (singleton (m1 + m2 - 1))
          where
            m1 = size states1
            m2 = size states2
            moves1'  = moves1
            moves2'  = mapMonotonic (renumberMove m1) moves2
            newmoves = singleton (Emove (m1 - 1) m1)


-- | Transform an NFA which recognizes language L1 to a new NFA
-- which recognizes L1*
nfaStar :: Nfa -> Nfa
nfaStar (NFA states1 moves1 _ _)
    = NFA (fromList [0..(m1 + 1)])
          (moves1' `union` newmoves)
          0
          (fromList [m1 + 1])
          where
            m1 = size states1
            moves1'  = mapMonotonic (renumberMove 1) moves1
            newmoves = fromList [Emove 0 1,
                                 Emove 0 (m1 + 1),
                                 Emove m1 1,
                                 Emove m1 (m1 + 1)]
-}
