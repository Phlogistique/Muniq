module Muniq (efficientTree, efficientFlat, bigFirst, smallFirst, bruteForce, noop, uniq, Uniqed(..)) where

import Data.List  (sort, sortBy, permutations, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord   (comparing)

import Muniq.Uniqed (Uniqed(..))
import Muniq.Utils (orBy)
import qualified Muniq.Uniqed as U
import qualified Muniq.Search as S

efficientTree, efficientFlat, noop, uniq :: (Eq a) => [a] -> [Uniqed a]
efficientTree syms = U.apt (S.ept $ sort $ S.findPatternsL syms) syms
bigFirst syms = U.apt (S.ept $ sortBy (S.patternLength `orBy` S.score) $ S.findPatternsL syms) syms
smallFirst syms = U.apt (S.ept $ sortBy ((*(-1)) . S.patternLength `orBy` S.score) $ S.findPatternsL syms) syms
bruteForce syms = U.apt (maximumBy (comparing S.scoreTree) $ map S.ept $ permutations $ S.findPatternsL syms) syms
efficientFlat syms = fromJust $ U.applyPatterns (S.efficientPatternsFlat $ S.findPatternsL syms) syms
noop = U.muniqNoop
uniq = U.uniq
