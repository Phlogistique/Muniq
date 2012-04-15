{-# LANGUAGE TemplateHaskell #-}
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary, suchThat, oneof, Positive(Positive), (==>))
import Test.HUnit ((@=?))

import Muniq
import qualified Data.Vector as V
import Control.Monad (liftM, liftM2, liftM3)
import Data.Maybe (isJust)
import Data.List (null, drop)

_prop_muniq :: ([Int] -> [Uniqed Int]) -> [Int] -> Bool
_prop_muniq f l = (qinum . f) l == l

prop_noop :: [Int] -> Bool
prop_noop = _prop_muniq muniqNoop

prop_flatten :: [Int] -> Bool
prop_flatten l = (muniqFlatten . muniqNoop) l == muniqNoop l

instance Arbitrary Pattern where
    arbitrary = let gt n = arbitrary `suchThat` (>= n)
                in liftM3 Pattern (gt 1) (gt 0) (gt 1)

shorterThan n = null . drop n
instance Arbitrary a => Arbitrary (Uniqed a) where
    arbitrary = oneof [ liftM Single arbitrary
                      , do Positive n <- arbitrary
                           g <- arbitrary `suchThat` shorterThan 10
                           return $ Group n g
                      ]


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

case_applyPattern1 = Just [Group 2 [Single 1, Single 2, Single 3]]
                 @=? applyPattern (Pattern 3 0 2) (muniqNoop [1,2,3,1,2,3])

case_applyPattern2 = Just [Group 2 [Single 1, Group 2 [Single 2, Single 3]]]
                 @=? applyPattern (Pattern 5 0 2) [Single 1, Group 2 [Single 2, Single 3],
                                                   Single 1, Group 2 [Single 2, Single 3]]

case_intersect = True @=? intersect (Pattern 10 0 2) (Pattern 10 5 1)

--prop_splitU1 (Positive n) u = isJust s ==> a ++ b == u
--  where
--    types = u :: [Uniqed Int]
--    s = splitU n u
--    Just (a,b) = s

case_lengthU = 10
           @=? lengthU [Group 2 [Single 1, Group 2 [Single 2, Single 3]]]

case_splitU0 = Just ([], [Group 2 [Single 3], Group 2 [Single 4]])
           @=? splitU 0 [Group 2 [Single 3], Group 2 [Single 4]]
case_splitU1 = Nothing
           @=? splitU 1 [Group 2 [Single 3], Group 2 [Single 4]]
case_splitU2 = Just ([Group 2 [Single 3]], [Group 2 [Single 4]])
           @=? splitU 2 [Group 2 [Single 3], Group 2 [Single 4]]
case_splitU3 = Nothing
           @=? splitU 3 [Group 2 [Single 3], Group 2 [Single 4]]
case_splitU4 = Just ([Group 2 [Single 3], Group 2 [Single 4]], [])
           @=? splitU 4 [Group 2 [Single 3], Group 2 [Single 4]]

main = $(defaultMainGenerator)

