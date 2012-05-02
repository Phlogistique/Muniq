{-# LANGUAGE PatternGuards #-}
module Muniq.Search where

-- Functions to find patterns in flat lists/vectors

import Control.Arrow (first, second)
import Data.Function (on)
import Data.List     (sort)
import Data.Tree     (Tree( Node ), Forest)
import Data.Vector   (Vector, (!))
import qualified Data.Vector as V

import Muniq.Utils

data Pattern = Pattern Int Int Int
--                       { patternLength :: Int
--                       , patternStart :: Int
--                       , patternTimes :: Int
--                       }
  deriving (Show,Eq)

instance Ord Pattern where compare = flip (compare `on` score)

score :: Pattern -> Int
score (Pattern l s t) = (t - 1) * l 

-- Two patterns intersect
intersect :: Pattern -> Pattern -> Bool
intersect p p' = intersect' p p' || intersect' p' p
intersect' p@(Pattern l s t) p'@(Pattern l' s' t') = let e = patEnd p
                                                         e' = patEnd p'
                                                     in s <= s' && s' < e || s < e' && e' <= e

-- Return a subordinate pattern with insane pre-conditions
--
-- Returns either wether or not there is an intersection, or a subpattern if
-- the pattern is in the first occurence of the parent pattern

subordinate :: Pattern -> Pattern -> Either Bool Pattern
subordinate p@(Pattern l s t) p'@(Pattern l' s' t') | s <= s' && s' + patLen p' <= s + l = Right $ Pattern l' (s'-s) t'
                                                    | intersect p p'                     = Left $ True
                                                    | otherwise                          = Left $ False

-- Uses "++" to preserve order
subordinates :: Pattern -> [Pattern] -> ([Pattern], [Pattern])
subordinates p = foldl subordinates' ([],[])
  where
    subordinates' acc p' = case subordinate p p' of
                             Right x' ->   first (++[x']) acc
                             Left True ->  acc
                             Left False -> second (++[p']) acc

ept :: [Pattern] -> Forest Pattern
ept xs = let (a,b) = partIntersect xs
         in ept' a b
ept' :: [Pattern] -> [Pattern] -> Forest Pattern
ept' (x:xs) rest = let (a,b) = subordinates x rest
                   in Node x (ept a) : ept' xs b
ept' [] _ = []

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

isPattern :: (Eq a) => Vector a -> Pattern -> Bool 
isPattern arr (Pattern l s t) = all eq $
                                    for (0...l) $ \offset ->
                                        for (0...t) $ \iter ->
                                            arr ! (s + offset + iter * l)

efficientPatternsFlat :: [Pattern] -> [Pattern]
efficientPatternsFlat = noIntersect . sort

noIntersect :: [Pattern] -> [Pattern]
noIntersect (x:xs) = x : noIntersect (filter (not . intersect x) xs)
noIntersect xs = xs

partIntersect :: [Pattern] -> ([Pattern], [Pattern])
partIntersect = partRel intersect

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
cutPattern p@(Pattern l s t) p' = case (cutPatternAt p' s, cutPatternAt p' $ patEnd p) of
                                       (Nothing          , Nothing                   ) -> [p']
                                       (Just (p1,p2)     , Nothing                   ) -> [p1,p2]
                                       (Nothing          , Just (p1,p2)              ) -> [p1,p2]
                                       (Just (p1,p2)     , Just (p3,p4)              ) -> [p1, p3, p4]

cutPatternF :: Pattern -> Pattern -> Forest Pattern
cutPatternF p@(Pattern l s t) p' = case (cutPatternAt p' s, cutPatternAt p' $ patEnd p) of
                                        (Nothing          , Nothing                   ) -> [Node p [], Node p' []]
                                        (Just (p1,p2)     , Nothing                   ) -> [Node p1 [], Node p [Node p2 []]]
                                        (Nothing          , Just (p1,p2)              ) -> [Node p [Node p1 []], Node p2 []]
                                        (Just (p1,p2)     , Just (p3,p4)              ) -> [Node p1 [], Node p [Node p3 []], Node p4 []]










