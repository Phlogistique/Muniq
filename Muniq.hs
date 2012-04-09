{-# LANGUAGE PatternGuards #-}
module Muniq where

import Control.Monad (liftM, foldM)
import Data.List     (tails, foldl', maximumBy)
import Data.Ord      (comparing)
import Data.Tree     (Tree( Node ), Forest)
import Data.Vector   ((!), Vector)
import qualified Data.Vector as V

data Uniqed a = Single a
              | Group Int [Uniqed a]
  deriving (Show,Eq)

-- Generic stuff
(.:) = (.).(.)
a ... b = [a .. b-1]
for = flip map

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

data Pattern = Pattern { patternLength :: Int
                       , patternStart :: Int
                       , patternTimes :: Int
                       }
  deriving (Show,Eq)

detectRepeat :: Eq a => Int -> Vector a -> [Maybe Pattern]
detectRepeat patlen arr = [ detectRepeat' offset | offset <- [0 .. (patlen - 1)] ]
  where
    detectRepeat' offset = case detectRepeat' (offset + patlen) of
                             Just (Pattern l s t) | arr ! offset == arr ! (offset + patlen) -> Just $ Pattern l offset (t+1)
                                                  | otherwise                               -> Just $ Pattern l s t
                             Nothing              | arr ! offset == arr ! (offset + patlen) -> Just $ Pattern patlen offset 2
                                                  | otherwise                               -> Nothing
eq (x:xs) = all (== x) xs
eq []     = True

allPatterns arrlen = [ p | patlen <- [1..arrlen `quot` 2]
                         , offset <- 0 ... patlen
                         , p      <- allPatterns' patlen arrlen offset ]

allPatterns' patlen arrlen offset = let maxlen = (arrlen - offset) `quot` patlen
                                    in [ let s = offset + n * patlen
                                         in Pattern patlen s t
                                       | n <- [0..maxlen]
                                       , t <- [2..maxlen - n] ]

findPatterns arr = filter (isPattern arr) $ allPatterns $ V.length arr

isPattern arr (Pattern l s t) = all eq $
                                    for (0...l) $ \offset ->
                                        for (0...t) $ \iter ->
                                            arr ! (s + offset + iter * l)

score :: Pattern -> Int
score (Pattern l s t) = (t - 1) * l 

-- Two patterns intersect
-- current implementation is WRONG!
intersect :: Pattern -> Pattern -> Bool
intersect p@(Pattern l s t) p'@(Pattern l' s' t') = let e = patEnd p
                                                        e' = patEnd p'
                                                    in s <= s' && s' < e || s <= e' && e' < e

ceilMul :: Integral a => a -> a -> a
ceilMul a b = a + let r = a `mod` b
                  in case r of 0 -> 0
                               _ -> b - r 

patEnd (Pattern l s t) = s + l * t
patLen (Pattern l s t) = l * t

cutPatternAt :: Pattern -> Int -> Maybe (Pattern, Pattern)
cutPatternAt p@(Pattern l s t) c | c < s || c >= patEnd p = Nothing
                                 | otherwise              = Just $ cutPatternAtUnsafe p c

cutPatternAtUnsafe :: Pattern -> Int -> (Pattern, Pattern)
cutPatternAtUnsafe p@(Pattern l s t) c = let before  = c - s
                                             t1      = before `div` l
                                             s2      = s + ceilMul before l
                                             after   = patEnd p - c
                                             t2      = after `div` l
                                         in (Pattern l s t1, Pattern l s2 t2)

cutPattern :: Pattern -> Pattern -> [Pattern]
cutPattern p@(Pattern l s t) p'
    | Nothing      <- cutPatternAt p' s
    , Nothing      <- cutPatternAt p' (patEnd p) = [p']
    | Just (p1,p2) <- cutPatternAt p' s
    , Nothing      <- cutPatternAt p' (patEnd p) = [p1, p2]
    | Nothing      <- cutPatternAt p' s
    , Just (p1,p2) <- cutPatternAt p' (patEnd p) = [p1, p2]
    | Just (p1,p2) <- cutPatternAt p' s
    , Just (p3,p4) <- cutPatternAt p2 (patEnd p) = [p1, p3, p4]

cutPatternF :: Pattern -> Pattern -> Forest Pattern
cutPatternF p@(Pattern l s t) p'
    | Nothing      <- cutPatternAt p' s
    , Nothing      <- cutPatternAt p' (patEnd p) = [Node p [], Node p' []]
    | Just (p1,p2) <- cutPatternAt p' s
    , Nothing      <- cutPatternAt p' (patEnd p) = [Node p1 [], Node p [Node p2 []]]
    | Nothing      <- cutPatternAt p' s
    , Just (p1,p2) <- cutPatternAt p' (patEnd p) = [Node p [Node p1 []], Node p2 []]
    | Just (p1,p2) <- cutPatternAt p' s
    , Just (p3,p4) <- cutPatternAt p2 (patEnd p) = [Node p1 [], Node p [Node p3 []], Node p4 []]

applyCouples :: (a -> a -> b) -> [a] -> [b]
applyCouples op xs = [ a `op` b | a:bs <- tails xs, b <- bs ]

anyCouple :: (a -> a -> Bool)  -> [a] -> Bool
anyCouple = or .: applyCouples

foldWithPatterns :: [a] -> [Pattern] -> Maybe [Uniqed a]
foldWithPatterns l pat | any (\x -> patEnd x > length l) pat = Nothing
foldWithPatterns _ pat | anyCouple intersect pat = Nothing
foldWithPatterns l pat = foldWithPatternsUnsafe l pat
                        
foldWithPatternsUnsafe :: [a] -> [Pattern] -> Maybe [Uniqed a]
foldWithPatternsUnsafe l p = foldM (flip applyPattern) (muniqNoop l) p

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
splitU 0 _ = Just ([],[])
splitU n [] = Nothing
splitU n (x:xs) = case lengthU' x of l | l == n -> return ([x],xs)
                                       | l < n  -> do (a,b) <- splitU (n - l) xs
                                                      return (x:a, b)
                                       | l > n  -> Nothing
takeU = liftM fst .: splitU
dropU = liftM snd .: splitU

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
                                                                  
efficientFirst :: [a] -> [Pattern] -> [Uniqed a]
efficientFirst l ps = let Just x = efficientFirst' (muniqNoop l) ps in x
  where
    efficientFirst' :: [Uniqed a] -> [Pattern] -> Maybe [Uniqed a]
    efficientFirst' l [] = Just $ muniqFlatten l
    efficientFirst' l ps = do let p = maximumBy (comparing score) ps
                                  ps' = concatMap (cutPattern p) ps
                              applied <- applyPattern p l
                              efficientFirst' applied ps'

