{-# LANGUAGE TemplateHaskell #-}
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary, suchThat)
import Test.HUnit ((@=?))

import Muniq
import qualified Data.Vector as V
import Control.Monad (liftM3)

_prop_muniq :: ([Int] -> [Uniqed Int]) -> [Int] -> Bool
_prop_muniq f l = (qinum . f) l == l

prop_noop :: [Int] -> Bool
prop_noop = _prop_muniq muniqNoop

prop_flatten :: [Int] -> Bool
prop_flatten l = (muniqFlatten . muniqNoop) l == muniqNoop l

instance Arbitrary Pattern where
    arbitrary = let gt n = arbitrary `suchThat` (>= n)
                in liftM3 Pattern (gt 1) (gt 0) (gt 1)

listExample = [3,4,5,5,5,4,5,5,5]
vecExample = V.fromList listExample

case_isPattern1 = True @=? isPattern vecExample (Pattern 1 2 3) 
case_isPattern2 = False @=? isPattern vecExample (Pattern 1 2 4)
case_isPattern3 = True @=? isPattern vecExample (Pattern 4 1 2)
case_findPatterns1 = True @=? (Pattern 1 2 3) `elem` findPatterns vecExample
case_allPatterns0 = [] @=? allPatterns 0
case_allPatterns1 = [] @=? allPatterns 1
case_allPatterns2 = [Pattern 1 0 2] @=? allPatterns 2

prop_intersect p@(Pattern l s t) p'@(Pattern l' s' t') = intersect p p' == let e = patEnd p
                                                                               e' = patEnd p'
                                                                           in s <= s' && s' < e || s <= e' && e' < e

case_intersect2 = True @=? intersect (Pattern 10 0 2) (Pattern 10 5 1)


main = $(defaultMainGenerator)

