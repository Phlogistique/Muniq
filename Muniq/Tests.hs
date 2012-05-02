{-# LANGUAGE TemplateHaskell #-}
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary, shrink, oneof, choose,
                        Positive(Positive), (==>), frequency)
import Test.HUnit ((@=?))

import qualified Data.Vector as V
import Control.Monad (liftM, liftM3)
import Data.Maybe (isJust)
import Data.Tree (Tree(Node), Forest)

import Muniq.Uniqed
import Muniq.Search
import Muniq.Utils

_prop_muniq :: ([Int] -> [Uniqed Int]) -> [Int] -> Bool
_prop_muniq f l = (qinum . f) l == l

prop_noop :: [Int] -> Bool
prop_noop = _prop_muniq muniqNoop

prop_flatten :: [Int] -> Bool
prop_flatten l = (muniqFlatten . muniqNoop) l == muniqNoop l

newtype Finite a = Finite [a]
  deriving (Show)

instance Arbitrary a => Arbitrary (Finite a) where
    arbitrary = liftM Finite $ oneof [ return []
                                     , do x <- arbitrary
                                          Finite xs <- arbitrary
                                          return (x:xs)
                                     ]

newtype Small a = Small a
  deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (Small a) where
    arbitrary = liftM Small $ frequency [ (1, return 0)
                                        , (5, do Small next <- arbitrary
                                                 return (next+1)) ]

instance Arbitrary Pattern where
    arbitrary = liftM3 Pattern (choose (1,10000)) (choose (0,10000)) (choose (0,10000))
    shrink (Pattern l s t) = filter (\(Pattern l s t) -> l > 1 && s > 0 && t > 1) 
                                    [Pattern l' s' t' | l' <- shrink l
                                                      , s' <- shrink s
                                                      , t' <- shrink t ]

instance Arbitrary a => Arbitrary (Uniqed a) where
    arbitrary = oneof [ liftM Single arbitrary
                      , do Positive n <- arbitrary
                           Finite g <- arbitrary
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

case_applyPattern1 = Just [Group 2 [Single 1, Single 2, Single 3]]
                 @=? applyPattern (Pattern 3 0 2) (muniqNoop [1,2,3,1,2,3])

case_applyPattern2 = Just [Group 2 [Single 1, Group 2 [Single 2, Single 3]]]
                 @=? applyPattern (Pattern 5 0 2) [Single 1, Group 2 [Single 2, Single 3],
                                                   Single 1, Group 2 [Single 2, Single 3]]

case_intersect1 = True @=? intersect (Pattern 10 0 2) (Pattern 10 5 1)
case_intersect2 = False @=? intersect (Pattern 10 0 1) (Pattern 10 10 1)

prop_splitU1 (Small n) (Finite u) = isJust s ==> a ++ b == u
  where
    types = u :: [Uniqed Int]
    s = splitU n u
    Just (a,b) = s

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

prop_partIntersect (Finite l) = let (distinct,rest) = partIntersect l
                                in not $ anyCouple intersect distinct

prop_subordinates1 p@(Pattern l s t) (Finite ps) = all (\p' -> patLen p' <= l) $ fst $ subordinates p ps
prop_subordinates2 p@(Pattern l s t) (Finite ps) = not $ any (intersect p) $ snd $ subordinates p ps

newtype PatForest = PatForest (Forest Pattern) deriving (Show)
instance Eq PatForest where
    (PatForest a) == (PatForest b) = sameContent compareNode a b
      where
        compareNode :: Eq a => Tree a -> Tree a -> Bool
        compareNode (Node a as) (Node b bs) = a == b && sameContent compareNode as bs

        sameContent :: (a -> a -> Bool) -> [a] -> [a] -> Bool
        sameContent (==) a b = sameContent' (==) a b []
        sameContent' (==) (x:xs) (y:ys) acc | x == y    = sameContent' (==) xs (ys ++ acc) []
                                            | otherwise = sameContent' (==) (x:xs) ys (y:acc)
        sameContent' (==) (x:xs) [] _ = False
        sameContent' (==) [] (y:ys) _ = False
        sameContent' (==) [] [] [] = True
        sameContent' (==) [] [] _ = False -- I don't think this ever happens

case_ept = PatForest [Node (Pattern 10 0 10) [Node (Pattern 2 0 2) [], Node (Pattern 2 6 2) []], Node (Pattern 10 1000 10) []]
       @=? PatForest (ept [Pattern 10 0 10, Pattern 2 0 2, Pattern 2 6 2, Pattern 2 6 8, Pattern 10 1000 10])

case_subordinate = Right (Pattern 2 2 3)
               @=? subordinate (Pattern 10 10 10) (Pattern 2 12 3)

main = $(defaultMainGenerator)

