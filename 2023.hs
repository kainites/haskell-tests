module 2023 where

import Data.List
import Data.Char

import Types
import WordData
import Clues
import Examples

------------------------------------------------------
-- Part I

punctuation :: String
punctuation
  = "';.,-!?"

cleanUp :: String -> String
cleanUp s = [toLower c | c <- s, c `notElem` punctuation]

split2 :: [a] -> [([a], [a])]
split2 xs = flip splitAt xs <$> [1..(length xs-1)]
-- split2 xs = scanl (\ (as, b) a -> (as ++ [a], tail b)) ([], xs) xs
-- takes the second argument ([], xs) and first item of the list (xs) and applies the function on them and feeds result back in, applying function to the result and second item of the list and so forth


split3 :: [a] -> [([a], [a], [a])]
split3 xs = concatMap (\ (a, b) -> (a, [], b):[(a1, a2, b) | (a1, a2) <- split2 a]) (split2 xs)
--                    applies this function to all values in                        this list

uninsert :: [a] -> [([a], [a])]
uninsert xs = [(b, a++c) | (a, b, c) <- split3 xs, not (null b)]

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs]
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs

------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches s (Synonym s') = s `elem` synonyms s'
matches s (Anagram _ s') = sort s == sort s'
matches s (Reversal _ t) = matches (reverse s) t 
matches s (Insertion _ t1 t2) = or [matches s1 t1 && matches s2 t2 | (s1, s2) <- uninsert s]
matches s (Charade _ t1 t2) = or [matches s1 t1 && matches s2 t2 | (s1, s2) <- split2 s]
matches s (HiddenWord _ s') = s == s'

evaluate :: Parse -> Int -> [String]
evaluate (def, link, t) i = [s | s <- filter ((== i) . length) (synonyms (unwords def)), matches s t]

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws,
            parseHidden ws]

parseSynonym :: [String] -> [ParseTree]
parseSynonym ws 
     | synonyms s == [] = []
     | otherwise        = [Synonym s]
     where s = unwords ws

parseAnagram :: [String] -> [ParseTree]
parseAnagram ws = [Anagram ind (concat arg) | (ind, arg) <- split2M ws, (unwords ind) `elem` anagramIndicators]

parseReversal :: [String] -> [ParseTree]
parseReversal ws = [Reversal ind pt | (ind, arg) <- split2M ws, pt <- (parseWordplay arg), (unwords ind) `elem` reversalIndicators]

parseInsertion :: [String] -> [ParseTree]
parseInsertion ws = [Insertion ind pt1 pt2 | (arg, ind, arg') <- split3 ws, pt1 <- (parseWordplay arg'), pt2 <- (parseWordplay arg), (unwords ind) `elem` envelopeIndicators]
                 ++ [Insertion ind pt1 pt2 | (arg, ind, arg') <- split3 ws, pt1 <- (parseWordplay arg), pt2 <- (parseWordplay arg'), (unwords ind) `elem` insertionIndicators]

parseCharade :: [String] -> [ParseTree]
parseCharade ws = [Charade ind pt1 pt2 | (arg, ind, arg') <- split3 ws, pt1 <- (parseWordplay arg), pt2 <- (parseWordplay arg'), (unwords ind) `elem` beforeIndicators]
               ++ [Charade ind pt1 pt2 | (arg, ind, arg') <- split3 ws, pt1 <- (parseWordplay arg'), pt2 <- (parseWordplay arg), (unwords ind) `elem` afterIndicators]

parseHidden :: [String] -> [ParseTree]
parseHidden ws = [HiddenWord ind st | (ind, arg) <- split2 ws, (st, _) <- uninsert (concat arg), not (st `elem` arg), (unwords ind) `elem` hiddenWordIndicators]

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText ws = [(def, link, pt) | (def, link, wp) <- split3M ws, pt <- parseWordplay wp, (unwords link) `elem` linkWords, length (synonyms (unwords def)) /= 0]
-- this gives 45 instead of 72 but checking against other solutions we always get 45?

solve :: Clue -> [Solution]
solve clue@(ws, i) = [(clue, p, s) | p <- parseClue clue, s <- evaluate p i]

------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]


