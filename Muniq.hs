module Muniq (efficientTree, efficientFlat, bigFirst, smallFirst, bruteForce, noop, Muniq.uniq, Uniqed(..)) where

import Data.List  (sort, sortBy, permutations, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord   (comparing)

import Muniq.Search (ept, findPatternsL, patternLength, efficientPatternsFlat, score, scoreTree)
import Muniq.Uniqed (Uniqed(..), apt, muniqNoop, uniq, applyPatterns)
import Muniq.Utils (Reverse(..))

efficientTree, efficientFlat, noop, uniq :: (Eq a) => [a] -> [Uniqed a]
efficientTree syms = apt (ept $ sort $ findPatternsL syms) syms
bigFirst syms = apt (ept $ sortBy (comparing $ \x -> (patternLength x, score x)) $ findPatternsL syms) syms
smallFirst syms = apt (ept $ sortBy (comparing $ \x -> (Reverse (patternLength x), score x)) $ findPatternsL syms) syms
bruteForce syms = apt (maximumBy (comparing scoreTree) $ map ept $ permutations $ findPatternsL syms) syms
efficientFlat syms = fromJust $ applyPatterns (efficientPatternsFlat $ findPatternsL syms) syms
noop = Muniq.Uniqed.muniqNoop
uniq = Muniq.Uniqed.uniq
