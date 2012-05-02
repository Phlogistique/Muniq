module Muniq.Uniqed where

-- Functions that manipulate symbol trees

import Control.Monad (liftM, foldM)
import Data.Maybe    (fromJust)
import Data.Tree     (Tree(Node), Forest)

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

-- This function tries to apply a Pattern, but may fail in the process
-- It tries only to apply the pattern to the upper-level layer of the tree
applyPattern :: Pattern -> [Uniqed a] -> Maybe [Uniqed a]
applyPattern = applyPattern' id

-- The actual implementation has one callback parameter that is useful for the
-- implementation of 'apt'
applyPattern' _ _ [] = Nothing
applyPattern' op p@(Pattern l s t) u = do (before,notbefore) <- splitU s u
                                          after <- dropU (patLen p) notbefore
                                          gu <- takeU l notbefore

                                          return $ before ++ [Group t $ op gu] ++ after

applyPatterns :: [Pattern] -> [a] -> Maybe [Uniqed a]
applyPatterns p = applyPatterns' p . muniqNoop

applyPatterns' :: [Pattern] -> [Uniqed a] -> Maybe [Uniqed a]
applyPatterns' p a = foldM (flip applyPattern) a p

-- apt: Apply Pattern Tree
-- Applies a Tree of patterns
apt :: Forest Pattern -> [a] -> [Uniqed a]
apt fp = apt' fp . muniqNoop
  where
    apt' :: Forest Pattern -> [Uniqed a] -> [Uniqed a]
    apt' (Node p children : xs) us = apt' xs $ fromJust $ applyPattern' (apt' children) p us
    apt' [] us = us

