module Muniq.Uniqed where

-- Functions that manipulate symbol trees

import Control.Monad (liftM, foldM)
import Data.List     (maximumBy)
import Data.Ord      (comparing)

import Muniq.Utils
import Muniq.Search

data Uniqed a = Single a
              | Group Int [Uniqed a]
  deriving (Show,Eq)

-- Applies a function to every node of an [Uniqed] tree
mumap = map . umap

umap :: (Uniqed a -> Uniqed a) -> Uniqed a -> Uniqed a
umap f (Single a) = f $ Single a
umap f (Group n a) = f $ Group n $ mumap f a

-- Implements the Noop strategy
muniqNoop :: [a] -> [Uniqed a]
muniqNoop = map Single

-- Remove redonding patterns. For example, transforms
-- 2×[ 2× [ 1 2 3 4 ] into 4×[ 1 2 3 4 ]
muniqFlatten = concatMap muniqFlatten'
  where
    muniqFlatten' (Group 1 x) = concatMap muniqFlatten' x
    muniqFlatten' (Group n x) = [Group n $ concatMap muniqFlatten' x]
    muniqFlatten' (Single x) = [Single x]

-- Flattens a [Uniqed] tree; can be used to check correctness.
qinum :: [Uniqed a] -> [a]
qinum = concatMap qinum' 
  where
    qinum' (Single a) = [a]
    qinum' (Group n u) = times n $ qinum u

times :: Int -> [a] -> [a]
times n x = concat (replicate n x)

uniq :: Eq a => [a] -> [Uniqed a]
uniq = uniq' . muniqNoop

uniq' (x:y:xs) | x == y = uniq' (Group 2 [x] : xs)
uniq' (Group n [x]:y:xs) | x == y = uniq' (Group (n+1) [x] : xs)
uniq' (x:xs) = x : uniq' xs
uniq' [] = []

lengthU :: [Uniqed a] -> Int
lengthU = foldl (+) 0 . map lengthU'

lengthU' (Single _)  = 1
lengthU' (Group r c) = r * lengthU c

takeU', takeU, dropU :: Int -> [Uniqed a] -> Maybe [Uniqed a]
takeU' 0 _ = Just [] 
takeU' n [] = Nothing
takeU' n (x:xs) = case lengthU' x of l | l == n -> return [x]
                                       | l < n  -> liftM (x:) $ takeU' (n - l) xs
                                       | l > n  -> let Group r c = x in takeU' n c

splitU :: Int -> [Uniqed a] -> Maybe ([Uniqed a],[Uniqed a])
splitU 0 xs = Just ([],xs)
splitU n [] = Nothing
splitU n (x:xs) = case lengthU' x of l | l == n -> return ([x],xs)
                                       | l < n  -> do (a,b) <- splitU (n - l) xs
                                                      return (x:a, b)
                                       | l > n  -> Nothing
takeU = liftM fst .: splitU
dropU = liftM snd .: splitU

-- Below, several functions try to apply a pattern. Not everyone of them make
-- sense.

-- This function tries to apply a Pattern, but may fail in the process
applyPattern :: Pattern -> [Uniqed a] -> Maybe [Uniqed a]
applyPattern _ [] = Nothing
applyPattern (Pattern l 0 t) (x:xs) = do taken <- takeU l xs
                                         dropped <- dropU (l * t) xs
                                         return (Group t taken : dropped)
applyPattern p@(Pattern l s t) (x:xs) = let lx = lengthU' x
                                        in if lx < s
                                           then let Group r c = x
                                                in applyPattern  p c
                                           else applyPattern (Pattern l (s - lx) t) xs

applyPatterns :: [Pattern] -> [a] -> Maybe [Uniqed a]
applyPatterns p = applyPatterns' p . muniqNoop

applyPatterns' :: [Pattern] -> [Uniqed a] -> Maybe [Uniqed a]
applyPatterns' p a = foldM (flip applyPattern) a p

-- Below is dead code
foldWithPatterns :: [a] -> [Pattern] -> Maybe [Uniqed a]
foldWithPatterns l pat | any (\x -> patEnd x > length l) pat = Nothing
foldWithPatterns _ pat | anyCouple intersect pat = Nothing
foldWithPatterns l pat = foldWithPatternsUnsafe l pat
                        
foldWithPatternsUnsafe :: [a] -> [Pattern] -> Maybe [Uniqed a]
foldWithPatternsUnsafe l p = foldM (flip applyPattern) (muniqNoop l) p

efficientFirst :: [a] -> [Pattern] -> [Uniqed a]
efficientFirst l ps = let Just x = efficientFirst' (muniqNoop l) ps in x
  where
    efficientFirst' :: [Uniqed a] -> [Pattern] -> Maybe [Uniqed a]
    efficientFirst' l [] = Just $ muniqFlatten l
    efficientFirst' l ps = do let p = maximumBy (comparing score) ps
                                  ps' = concatMap (cutPattern p) ps
                              applied <- applyPattern p l
                              efficientFirst' applied ps'

