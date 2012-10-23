module NFA (Nfa(NFA), Move(Move, Emove), run, accepts, nfaUnion, nfaConcat,
    nfaStar) where


import Data.Set as S


data Nfa a = NFA (Set a) (Set (Move a)) a (Set a)
    deriving (Eq, Show)

data Move a = Move a Char a
            | Emove a a
    deriving (Eq, Ord, Show)


-- | Keep iterating until a static set remains
setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
    | s == next = s
    | otherwise = setlimit f next
      where
        next = f s


-- | Compute the set of states reachable from epsilon transitions
closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA _ moves _ _) = setlimit add
  where
    add states = states `union` accessible
      where
        accessible = fromList [s |  x <- toList states,
                                    Emove y s <- toList moves,
                                    y == x]


-- | Compute the set of states reachable from symbolic transitions
onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA _ ms _ _) c x = fromList [s |  t <- toList x,
                                            Move z d s <- toList ms,
                                            z == t, c == d]


-- | Compute the state of the machine after a single transition
onetrans :: Ord a => Nfa a -> Set a -> Char -> Set a
onetrans m x c = closure m (onemove m c x)


-- | Compute the final state of an NFA on a string
run :: Ord a => Nfa a -> String -> Set a
run m@(NFA _ _ q _) = Prelude.foldl (onetrans m) (closure m (singleton q))


-- | Check if a NFA accepts a word
accepts :: Ord a => Nfa a -> String -> Bool
accepts m@(NFA _ _ _ fs) = not . S.null . S.intersection fs . run m


-- | Map a transition to a larger graph
renumberMove :: (Num a) => a -> Move a -> Move a
renumberMove i (Move a c b) = Move (a + i) c (b + i)
renumberMove i (Emove a b)  = Emove (a + i) (b + i)


-- | Combine two NFAs which recognize languages L1 and L2 to form a new
-- NFA which recognizes L1 | L2.
nfaUnion :: Nfa Int -> Nfa Int -> Nfa Int
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
nfaConcat :: Nfa Int -> Nfa Int -> Nfa Int
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
nfaStar :: Nfa Int -> Nfa Int
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
