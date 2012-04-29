import Control.Monad
import Data.Char
import Data.List hiding (union)
import Data.Set as S


data Nfa a = NFA (Set a) (Set (Move a)) a (Set a)
    deriving (Eq, Show)
data Move a = Move a Char a
            | Emove a a
    deriving (Eq, Ord, Show)


-- Keep iterating until a static set remains
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
    add states = union states accessible
      where
        accessible = fromList [s | x <- toList states, Emove y s <- toList moves, y == x]


-- | Compute the set of states reachable from symbolic transitions
onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA _ ms _ _) c x = fromList [s | t <- toList x, Move z d s <- toList ms, z == t, c == d]


-- | Compute the state of the machine after a single transition
onetrans :: Ord a => Nfa a -> Set a -> Char -> Set a
onetrans m x c = closure m (onemove m c x)


-- | Compute the final state of an NFA on a string
trans :: Ord a => Nfa a -> String -> Set a
trans m@(NFA _ _ q _) = foldl (onetrans m) (closure m (singleton q))


-- | Check if a NFA accepts a word
accepts :: Ord a => Nfa a -> String -> Bool
accepts m@(NFA qs ms q fs) = not . S.null . S.intersection fs . trans m


data Reg = Epsilon
         | Literal Char
         | Or Reg Reg
         | Concat Reg Reg
         | Star Reg
    deriving Eq


-- | Display a human-readable regex
showReg :: Reg -> [Char]
showReg Epsilon        = "@"
showReg (Literal c)    = [c]
showReg (Or r1 r2)     = "(" ++ showReg r1 ++  "|" ++ showReg r2 ++ ")"
showReg (Concat r1 r2) = "(" ++ showReg r1 ++ showReg r2 ++ ")"
showReg (Star r)       = showReg r ++ "*"


instance Show Reg where
    show = showReg


-- | Convert a postfix regex expression into a parse tree
evalPostfix :: String -> Reg
evalPostfix = head . foldl comb []
    where
        comb :: [Reg] -> Char -> [Reg]
        comb (x:y:ys) '|'   = (Or y x) : ys
        comb (x:y:ys) '&'   = (Concat y x) : ys
        comb (x:xs) '*'     = (Star x) : xs
        comb xs '@'         = Epsilon : xs
        comb xs s           = (Literal s) : xs


-- | Apply the shunting-yard algorithm to turn an infix expression
-- into a postfix expression.
-- Regexs must used implicit concatenations
shunt :: String -> String -> String -> String
shunt o p [] = (reverse o) ++ p
shunt o [] (x:xs)
    | x == '(' = shunt o [x] xs
    | x == '|' = shunt o [x] xs
    | x == '&' = shunt o [x] xs
    | x == '*' = shunt (x:o) [] xs
    | otherwise = shunt (x:o) [] xs
shunt o (p:ps) (x:xs)
    | x == '(' = shunt o (x:p:ps) xs
    | x == ')' = case (span (/= '(') (p:ps)) of
        (as, b:bs) -> shunt (as ++ o) bs xs
    | x == '|' = case (p) of
        '(' -> shunt o (x:p:ps) xs
        otherwise -> shunt (p:o) (x:ps) xs
    | x == '&' = shunt (o) (x:p:ps) xs
    | x == '*' = shunt (x:o) (p:ps) xs
    | otherwise = shunt (x:o) (p:ps) xs


-- | Convert an infix expression to postfix
toPostfix :: String -> String
toPostfix = shunt [] []


-- | Evaluate an infix expression
eval :: String -> Reg
eval = evalPostfix . toPostfix


-- | Map a state to a new graph
renumber :: (Num a) => a -> a -> a
renumber i = (+ i)


-- | Map a transition to a larger graph
renumber_move :: (Num a) => a -> Move a -> Move a
renumber_move i (Move a c b) = (Move (a + i) c (b + i))
renumber_move i (Emove a b)  = (Emove (a + i) (b + i))


-- | Combine two NFAs which recognize languages L1 and L2 to form a new
-- NFA which recognizes L1 | L2.
nfaUnion :: Nfa Int -> Nfa Int -> Nfa Int
nfaUnion (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA (fromList [0..(m1+m2+1)])
          (moves1' `union` moves2' `union` newmoves)
          0
          (singleton (m1 + m2 + 1))
          where
            m1 = size states1
            m2 = size states2
            moves1'  = mapMonotonic (renumber_move 1)        moves1
            moves2'  = mapMonotonic (renumber_move (m1 + 1)) moves2
            newmoves = fromList [Emove 0 1,
                                Emove 0 (m1 + 1),
                                Emove m1 (m1 + m2 + 1),
                                Emove (m1 + m2) (m1 + m2 + 1)]


-- | Combine two NFAs which recognize languages L1 and L2 to form a new
-- NFA which recognizes L1 + L2.
nfaConcat :: Nfa Int -> Nfa Int -> Nfa Int
nfaConcat (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA (fromList [0..(m1 + m2 - 1)])
          (moves1' `union` moves2' `union` newmoves)
          0
          (singleton (m1 + m2 - 1))
          where
            m1 = size states1
            m2 = size states2
            moves1'  = moves1
            moves2'  = mapMonotonic (renumber_move m1) moves2
            newmoves = singleton (Emove (m1 - 1) m1)


-- | Transform an NFA which recognizes language L1 to a new NFA
-- which recognizes L1*
nfaStar :: Nfa Int -> Nfa Int
nfaStar (NFA states1 moves1 start1 finish1)
    = NFA (fromList [0..(m1 + 1)])
          (moves1' `union` newmoves)
          0
          (fromList [m1 + 1])
          where
            m1 = size states1
            moves1'  = mapMonotonic (renumber_move 1) moves1
            newmoves = fromList [Emove 0 1,
                                 Emove 0 (m1 + 1),
                                 Emove m1 1,
                                 Emove m1 (m1 + 1)]


-- | Build a NFA from a regular expression
build :: Reg -> Nfa Int
build (Literal c)    = NFA (fromList [0, 1]) (singleton (Move 0 c 1)) 0 (singleton 1)
build (Or r1 r2)     = nfaUnion (build r1) (build r2)
build (Concat r1 r2) = nfaConcat (build r1) (build r2)
build (Star r)       = nfaStar (build r)


-- | Check if a regex matches a word
matches :: String -> String -> Bool
matches s = accepts (build (eval s))
