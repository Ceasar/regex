import Control.Monad
import Data.Char
import Data.List hiding (union)
import Data.Set


data Reg = Epsilon |
           Literal Char |
           Or Reg Reg |
           Concat Reg Reg |
           Star Reg
           deriving Eq


showReg :: Reg -> [Char]
showReg Epsilon        = "@"
showReg (Literal c)    = [c]
showReg (Or r1 r2)     = "(" ++ showReg r1 ++  "|" ++ showReg r2 ++ ")"
showReg (Concat r1 r2)   = "(" ++ showReg r1 ++ showReg r2 ++ ")"
showReg (Star r)       = showReg r ++ "*"


instance Show Reg where
    show = showReg


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


-- | Map a state to a new graph
renumber :: (Num a) => a -> a -> a
renumber i = (+ i)


-- | Map a move to a larger graph
renumber_move :: (Num a) => a -> Move a -> Move a
renumber_move i (Move a c b) = (Move (a + i) c (b + i))
renumber_move i (Emove a b)  = (Emove (a + i) (b + i))


m_or :: Nfa Int -> Nfa Int -> Nfa Int
m_or (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA (states1' `union` states2' `union` newstates)
          (moves1' `union` moves2' `union` newmoves)
          0
          (singleton (m1 + m2 + 1))
          where
            m1 = size states1
            m2 = size states2
            states1' = mapMonotonic (renumber 1)        states1
            states2' = mapMonotonic (renumber (m1 + 1)) states2
            newstates = fromList [0..(m1 + m2 + 1)]
            moves1' = mapMonotonic (renumber_move 1)        moves1
            moves2' = mapMonotonic (renumber_move (m1 + 1)) moves2
            newmoves = fromList [Emove 0 1,
                                Emove 0 (m1 + 1),
                                Emove m1 (m1 + m2 + 1),
                                Emove (m1 + m2) (m1 + m2 + 1)]


build :: Reg -> Nfa Int
build (Literal c) = NFA (fromList [0, 1]) (singleton (Move 0 c 1)) 0 (singleton 1)
build (Or r1 r2) = m_or (build r1) (build r2)
-- build (Then r1 r2) = m_then (build r1) (build r2)
-- build (Star r) = m_star (build r)
