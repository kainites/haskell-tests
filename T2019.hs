module T2019 where

import Data.List
import Data.Maybe

import Types ( IdMap, CNFRep, CNF, NNF, Formula(..), Id )
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp i ((k, v):ps)
   | i == k      = v
   | otherwise   = lookUp i ps

-- 3 marks
vars :: Formula -> [Id]
vars (Var v) = [v]
vars (Not f) = vars f
vars (And f g) = nub (sort (vars f ++ vars g))
vars (Or f g) = nub (sort (vars f ++ vars g))

-- 1 mark
idMap :: Formula -> IdMap
idMap f = [(x, fromJust (elemIndex x li) + 1) | x <- li]
  where li = vars f

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Var v) = Var v
toNNF (Not (Not f)) = toNNF f
toNNF (Not (And f g)) = toNNF (Or (Not f) (Not g))
toNNF (Not (Or f g)) = toNNF (And (Not f) (Not g))
toNNF (Not v) = Not v
toNNF (Or f g) = Or (toNNF f) (toNNF g)
toNNF (And f g) = And (toNNF f) (toNNF g)

-- 3 marks
toCNF :: Formula -> CNF
toCNF f = go f'
  where
    f' = toNNF f
    -- go (Or (And f g) h) = go (And (Or f h) (Or g h))
    -- go (Or f (And g h)) = go (And (Or f g) (Or f h))
    -- go (Or f g) = Or (go f) (go g)
    go (Var v) = Var v
    go (Not v) = Not v
    go (And f g) = And (go f) (go g)
    go (Or f g) = distribute f g

-- 4 marks
flatten :: CNF -> CNFRep
flatten f = go f
  where m = idMap f
        go (Var v) = [[lookUp v m]]
        go (Not (Var v)) = [[-(lookUp v m)]]
        go (And f g) = go f ++ go g
        go (Or f g) = [head (go f) ++ head (go g)]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits x = go x x'
  where
    x' = getUnitClause x
    go [] u = ([], u)
    go ([x]:xs) u
      | x `elem` u = go xs u
    go (x:xs) u
      | null (x `intersect` u) = if length filt == 1
                                 then go xs u'
                                 else (filt : fst (go xs u'), u')
      | otherwise              = go xs u
          where filt = [y | y <- x, (- y) `notElem` u]
                u' = u ++ getUnitClause [filt]


getUnitClause :: CNFRep -> [Int]
getUnitClause = concat . filter ((== 1) . length)

-- 4 marks
dp :: CNFRep -> [[Int]]
dp x = undefined
-- fuck this 

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


