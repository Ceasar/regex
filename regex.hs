import Control.Monad
import Data.Char
import Data.List hiding (union)
import Data.Set as S


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


data Nfa a = NFA (Set a) (Set (Move a)) a (Set a)
    deriving (Eq, Show)
data Move a = Move a Char a
            | Emove a a
    deriving (Eq, Ord, Show)


-- | Get all of the transitions of a state
out :: Ord a => Set (Move a) -> a -> Set (Move a)
out moves q = S.filter pred moves
  where
    pred (Move u _ _) = q == u
    pred (Emove u _) = q == u


-- | Check if a transition is an epsilon transition
isEpsilon :: Move a -> Bool
isEpsilon (Move _ _ _) = False
isEpsilon (Emove _ _) = True


-- | Get all of the epsilon transitions of a state
ep :: Ord a => Nfa a -> a -> Set (Move a)
ep (NFA _ ms _ _) = S.filter isEpsilon . out ms


-- | Get the transition function of an nfa
delta :: Ord a => Set (Move a) -> a -> Char -> Set a
delta ms state char = S.fold comb S.empty ms
    where
        comb (Move u d v) vs
            | u == state && d == char = S.insert v vs
            | otherwise = vs
        comb (Emove u v) vs
            | u == state = S.insert v vs
            | otherwise = vs


-- >>= for Sets.
bindSet :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
bindSet s k = S.unions . S.toList $ S.map k s


-- foldM for Sets.
foldSet :: (Ord a ) => (a -> b -> Set a) -> a -> [b] -> Set a
foldSet _ a []     = S.singleton a
foldSet f a (x:xs) = bindSet (f a x) (\fax -> foldSet f fax xs)


-- | Check if a NFA accepts a word
accepts :: Ord a => Nfa a -> String -> Bool
accepts (NFA qs ms q fs) = not . S.null . S.intersection fs . foldSet (delta ms) q


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
          (fromList [m1, m1 + 1])
          where
            m1 = size states1
            moves1'  = mapMonotonic (renumber_move 1) moves1
            newmoves = fromList [Emove 0 1,
                                 Emove m1 1,
                                 Emove m1 (m1 + 1),
                                 Emove (m1 + 1) 0]


-- | Build a NFA from a regular expression
build :: Reg -> Nfa Int
build (Literal c)    = NFA (fromList [0, 1]) (singleton (Move 0 c 1)) 0 (singleton 1)
build (Or r1 r2)     = nfaUnion (build r1) (build r2)
build (Concat r1 r2) = nfaConcat (build r1) (build r2)
build (Star r)       = nfaStar (build r)


-- | Check if a regex matches a word
matches :: String -> String -> Bool
matches s = accepts (build (eval s))
