module Muniq.Utils where

-- Generic stuff

import Data.List (tails, partition)
import Data.Ord (comparing)

(.:) = (.).(.)
a ... b = [a .. b-1]
for = flip map

eq (x:xs) = all (== x) xs
eq []     = True

-- applyCouples (,) [1,2,3] == [(1,2),(1,3),(2,3)]
applyCouples :: (a -> a -> b) -> [a] -> [b]
applyCouples op xs = [ a `op` b | a:bs <- tails xs, b <- bs ]

anyCouple :: (a -> a -> Bool) -> [a] -> Bool
anyCouple = or .: applyCouples

-- Given 'op' a commutative predicate on two values of type a, 'partRel op xs'
-- returns (as,rest), for which,
-- * for every couple of distinct values (a1,a2) taken in 'xs', a1 and a2 are in 'as' iff 'a1 `op` a2 == False'
-- * for any a1 in 'rest', there is an a2 in 'as' for which 'a2 `op` a1'
-- * 'as ++ rest' is a permutation of 'xs' (contains the same elements)
--
-- More intuitively, if 'op' defines intersection between elements of 'xs',
-- partRel returns a sublist without any intersection (as) and then rest of the list.
--
-- parRel *should* preserve order. TODO: prove it.
partRel :: (a -> a -> Bool) -> [a] -> ([a],[a])
partRel op (x:xs) = let (t,f) = partition (op x) xs
                        (t',f') = partRel op f
                    in (x : t', t ++ f')
partRel _ xs = (xs,[])

newtype Reverse a = Reverse a
instance Eq a => Eq (Reverse a) where (Reverse a) == (Reverse b) = a == b
instance Ord a => Ord (Reverse a) where
    compare (Reverse a) (Reverse b) = compare b a
