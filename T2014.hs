module T2014 where
import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp x = fromJust . lookup x
-- lookUp x = (fromJust .) . lookup

simplify :: RE -> RE
-- simplify (Plus x) = Seq y (Rep y)
--    where y = simplify x
-- simplify (Opt x) = Alt y Null
--    where y = simplify x
simplify (Plus x) = simplify (Seq x (Rep x))
simplify (Opt x) = simplify (Alt x Null)
simplify (Seq x y) = Seq (simplify x) (simplify y)
simplify (Alt x y) = Alt (simplify x) (simplify y)
simplify (Rep x) = Rep (simplify x)
simplify x = x

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (x, _, _) = x
terminalStates :: Automaton -> [State]
terminalStates (_, y, _) = y
transitions :: Automaton -> [Transition]
transitions (_, _, z) = z

isTerminal :: State -> Automaton -> Bool
isTerminal s a = s `elem` terminalStates a
-- (. terminalStates) . elem

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a = [t | t@(s1, _, _) <- transitions a, s == s1]

labels :: [Transition] -> [Label]
labels ts = nub [C l | (_, _, C l) <- ts] 
-- (_, _, l) <- ts, l /= Eps

accepts :: Automaton -> String -> Bool
accepts a cs = accepts' s cs
   where
    s = startState a

    accepts' :: State -> String -> Bool
    accepts' s cs
      | null cs && isTerminal s a = True
      | otherwise                 = any (try cs) (transitionsFrom s a)

    try :: String -> Transition -> Bool
    try cs (_, t, Eps) = accepts' t cs
    try "" _ = False
    try (c':cs) (_, t, C c)
      | c' == c   = accepts' t cs
      | otherwise = False

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k = ([(m, n, Eps)], k)
make (Term c) m n k = ([(m, n, C c)], k)
make (Seq r1 r2) m n k = (fst (make r1 m k (k+2)) ++ 
  [(k, k+1, Eps)] ++ fst (make r2 (k+1) n (k+2)), k+2)
make (Alt r1 r2) m n k = ([(m, k, Eps), (m, k+2, Eps), 
  (k+1, n, Eps), (k+3, n, Eps)] ++ fst (make r1 k (k+1) (k+4)) 
  ++ fst (make r2 (k+2) (k+3) (k+4)), k+4)
make (Rep r) m n k = ([(m, n, Eps), (m, k, Eps), (k+1, n, Eps), 
  (k+1, k, Eps)] ++ fst (make r k (k+1) (k+4)), k+1)

--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier s nda 
  | isTerminal s nda = [(s, s, Eps)]
  | otherwise = concatMap getFrontier' (transitionsFrom s nda)
  where 
    getFrontier' (_, s', Eps) = getFrontier s' nda 
    getFrontier' t = [t]

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions ts = [(lab, [s' | (_, s', lab') <- ts, lab == lab']) | lab <- labels ts]

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
-- fuck this
makeDA
  = undefined

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

