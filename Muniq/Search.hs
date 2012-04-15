{-# LANGUAGE PatternGuards #-}
module Muniq.Search where

-- Functions to find patterns in flat lists/vectors

import Data.List     (sort)
import Data.Ord      (comparing)
import Data.Tree     (Tree( Node ), Forest)
import Data.Vector   ((!), Vector)
import qualified Data.Vector as V

import Muniq.Utils

data Pattern = Pattern { patternLength :: Int
                       , patternStart :: Int
                       , patternTimes :: Int
                       }
  deriving (Show,Eq)

instance Ord Pattern where compare = comparing score

score :: Pattern -> Int
score (Pattern l s t) = (t - 1) * l 

-- Two patterns intersect
-- WARNING: only checks if some part of p' is in p; not the opposite.
intersect :: Pattern -> Pattern -> Bool
intersect p p' = intersect' p p' || intersect' p' p
intersect' p@(Pattern l s t) p'@(Pattern l' s' t') = let e = patEnd p
                                                         e' = patEnd p'
                                                     in s <= s' && s' < e || s <= e' && e' < e

-- ceils a to a multiple of b
ceilMul :: Integral a => a -> a -> a
ceilMul a b = a + let r = a `mod` b
                  in case r of 0 -> 0
                               _ -> b - r 

patEnd (Pattern l s t) = s + l * t
patLen (Pattern l s t) = l * t


allPatterns arrlen = [ p | patlen <- [1..arrlen `quot` 2]
                         , offset <- 0 ... patlen
                         , p      <- allPatterns' patlen arrlen offset ]

allPatterns' patlen arrlen offset = [ let s = offset + n * patlen
                                      in Pattern patlen s t
                                    | n <- [0..maxlen]
                                    , t <- [2..maxlen - n] ]
  where
    maxlen = (arrlen - offset) `quot` patlen

findPatternsL :: Eq a => [a] -> [Pattern]
findPatternsL = findPatterns . V.fromList
findPatterns arr = filter (isPattern arr) $ allPatterns $ V.length arr

isPattern arr (Pattern l s t) = all eq $
                                    for (0...l) $ \offset ->
                                        for (0...t) $ \iter ->
                                            arr ! (s + offset + iter * l)

efficientPatternsFlat :: [Pattern] -> [Pattern]
efficientPatternsFlat = noIntersect . sort

noIntersect :: [Pattern] -> [Pattern]
noIntersect (x:xs) = x : noIntersect (filter (not . intersect x) xs)
noIntersect xs = xs

-- What follows is dead code
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
