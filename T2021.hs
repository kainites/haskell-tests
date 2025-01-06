module T2021 where

import Data.Maybe
import Data.List

import Types
    ( IdMap,
      Id,
      CFG,
      Colouring,
      Colour,
      IG,
      Graph,
      Function,
      Block,
      Statement(..),
      Exp(..) )
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x ys = sum [1 | y <- ys, y == x]


degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es) = [(n, cedge n es) | n <- ns]
   where
    cedge x ys = sum [1 | (a, b) <- ys, x == a || x == b]

neighbours :: Eq a => a -> Graph a -> [a]
neighbours t (ns, es) = [n | n <- ns, e <- es, (n, t) == e || (t, n) == e]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode t (ns, es) = (delete t ns, es')
   where es' = [e | e@(a, b) <- es, a /= t && b /= t]

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph cMax g = go g cs []
   where
    cs = [1..cMax]

    go :: (Ord a, Show a) => Graph a -> [Int] -> Colouring a -> Colouring a
    go ([n], _) cl _ = [(n, minimum cl)]
    go gr@(n:ns, es) cl cMap = (n, getCol n gr cl x) : x
       where x = go (removeNode n gr) cl cMap

    getCol :: (Ord a, Show a) => a -> Graph a -> [Int] -> Colouring a -> Colour
    getCol _ ([n], _) cl _ = minimum cl
    getCol n g cl cs = if null y
                       then 0
                       else minimum y

      where y = [c | c <- cl, c `notElem` nCols (neighbours n g) cs]
            nCols :: (Ord a, Show a) => [a] -> [(a, b)] -> [b]
            nCols ns x = [b | (a, b) <- x, n <- ns, n == a]

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap cs = ("return", "return") : [(v, check y) | y@(v, c) <- cs]
  where check (v, c) = if c == 0
                       then v
                       else 'R' : show c


buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments as i = [Assign y (Var a) | a <- as, (x, y) <- i, a == x]


renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const x) _ = Const x
renameExp (Var x) m = Var (fromJust (lookup x m))
renameExp (Apply op e1 e2) m = Apply op (renameExp e1 m) (renameExp e2 m)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b m = filter help $ map renameStatement b
  where
    help (Assign x (Var y)) = x /= y
    help _ = True
    renameStatement (Assign i e) = Assign (fromJust (lookup i m)) (renameExp e m)
    renameStatement (If e b1 b2) = If (renameExp e m) (renameBlock b1 m) (renameBlock b2 m)
    renameStatement (While e b) = While (renameExp e m) (renameBlock b m)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG lvs = (nub [v | l <- lvs, v <- l], nub [p | lv <- lvs, p <- pairUp lv])
    where pairUp [] = [] 
          pairUp [x] = []
          pairUp (x:xs) = withAll x xs ++ pairUp xs

          withAll y [x] = [(y, x)]
          withAll y (x:xs) = (y, x) : withAll y xs 

-----------------------------------------------------
--
-- Part V
-- fuck this
liveVars :: CFG -> [[Id]]
liveVars
  = undefined

buildCFG :: Function -> CFG
buildCFG
  = undefined
