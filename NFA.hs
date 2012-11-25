module NFA (Nfa(NFA), State, run, accepts, {-nfaUnion, nfaConcat, nfaStar-}) where


import Data.Set as S

type State = Int

data ST a = S (State -> (a, State))

instance Monad ST where
    -- return :: a -> ST a
    return x   = S (\s -> (x,s))

    -- (>>=)  :: ST a -> (a -> ST b) -> ST b
    st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')
        where
            apply        :: ST a -> State -> (a,State)
            apply (S h) = h

data Nfa = NFA  {   states   :: Set State, -- Set of states
                    relation :: Maybe Char -> State -> Set State, -- Relation
                    current  :: Set State, -- Current states
                    finals   :: Set State -- Final states
                }


testNFA :: Nfa
testNFA = NFA (fromList [0, 1, 2]) r (singleton 0) (singleton 1)
    where
        r :: Maybe Char -> State -> Set State
        r (Just 'a') 0 = singleton 1
        r (Just 'b') 0 = singleton 0
        r (Just 'a') 1 = singleton 1
        r (Just 'b') 1 = singleton 0
        r _ _ = singleton 2

{-
instance Monad Nfa where
    return x = undefined
    m >>= f =  undefined
-}

instance Show Nfa where
    show (NFA qs _ q fs) = concat ["NFA ", show qs, ",", show q, ",", show fs]


-- | Compute the set of states reachable from epsilon transitions
closure :: Nfa -> Nfa
closure (NFA qs f q fs) = NFA qs f q' fs
    where
        -- | Keep iterating until a static set remains
        setlimit :: (Set State -> Set State) -> Set State -> Set State
        setlimit g s
            | s == next = s
            | otherwise = setlimit g next
              where
                next = g s
        q' = setlimit (S.fold (S.union . f Nothing) q) q


-- | Compute the state of the machine after a single transition
onetrans ::  Char -> Nfa -> Nfa
onetrans c (NFA qs f q fs) = closure $ NFA qs f q' fs
    where
        q' = S.fold (S.union . f (Just c) ) S.empty q


-- | Compute the final state of an NFA on a string
run :: Nfa -> String -> Nfa
run m = Prelude.foldr onetrans (closure m)


-- | Check if a NFA accepts a word
accepts :: Nfa -> String -> Bool
accepts m@(NFA _ _ _ fs) = not . S.null . S.intersection fs . current . run m


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
