module Muniq.Utils where

-- Generic stuff

import Data.List (tails, partition)

(.:) = (.).(.)
a ... b = [a .. b-1]
for = flip map

eq (x:xs) = all (== x) xs
eq []     = True

applyCouples :: (a -> a -> b) -> [a] -> [b]
applyCouples op xs = [ a `op` b | a:bs <- tails xs, b <- bs ]

anyCouple :: (a -> a -> Bool) -> [a] -> Bool
anyCouple = or .: applyCouples

-- Given 'op' a commutative predicate on two values of type a, 'partRel op xs'
-- returns a list of values for which, for every couple of distinct values
-- (a1,a2) taken in 'xs', 'a1 `op` a2 == False'
partRel :: (a -> a -> Bool) -> [a] -> ([a],[a])
partRel op (x:xs) = let (t,f) = partition (op x) xs
                        (t',f') = partRel op f
                    in (x : t', t ++ f')
partRel _ xs = (xs,[])
