module T2020 where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes i = sum (convertToBit i)

convertToBit :: Int -> [Int] 
convertToBit 0 = [0]
convertToBit 1 = [1]
convertToBit x = [(x `mod` 2)] ++ convertToBit (x `div` 2)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n = sum (snd (splitAt (length b + 1 - i) b))
                    where b = convertToBit n

getIndex :: Int -> Int -> Int -> Int
getIndex int 0 size = int `mod` bit size
getIndex int n size = getIndex (int `div` bit size) (n - 1) size

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x:xs) y = y : xs 
replace i (x:xs) y = x : replace (i-1) xs y

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y x = y : x
insertAt i y (x:xs) = x : insertAt (i-1) y xs 

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ g (Leaf is) = g is
sumTrie f g (Node bv sns) = sum (map (sumSubNode f g) sns)

sumSubNode :: (Int -> Int) -> ([Int] -> Int) -> SubNode -> Int 
sumSubNode f _ (Term i) =  f i 
sumSubNode f g (SubTrie t) = sumTrie f g t 

trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member v h t bs = go 0 t 
  where 
    go _ (Leaf is) = v `elem` is 
    go level (Node bv sns) 
       | not (testBit bv i) = False 
       | Term v' <- sn      = v' == v 
       | SubTrie t' <- sn   = member v (h `div` bit bs) t' bs 
       where i = getIndex h level bs
             sn = sns !! countOnesFrom i bv
             
--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf md bs v t = go 0 v t
   where 
    go _ v (Leaf is) = if v `elem` is 
                       then Leaf is 
                       else Leaf (v:is)
    go l v (Node bv sns) 
       | l >= (md-1)        = Leaf [v]
       | not (testBit bv i) = Node (setBit bv i) (insertAt oi (Term v) sns)
       | SubTrie st <- sn   = Node bv (replace oi sns (SubTrie (go (l+1) v st)))
       | Term v' <- sn      = if v == v' 
                              then Node bv sns 
                              else Node bv (replace oi sns (SubTrie (go (l+1) v (go (l+1) v' empty))))
       where i = getIndex (hf v) l bs 
             sn = sns !! oi 
             oi = countOnesFrom i bv
-- *** Exception: Prelude.!!: index too large
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/List.hs:1368:14 in base:GHC.List
--   tooLarge, called at libraries/base/GHC/List.hs:1378:50 in base:GHC.List
--   !!, called at Tries.hs:115:18 in main:Tries

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf md bs = foldr (insert hf md bs) empty