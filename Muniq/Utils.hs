module Muniq.Utils where

-- Generic stuff

import Data.List (tails)

(.:) = (.).(.)
a ... b = [a .. b-1]
for = flip map

eq (x:xs) = all (== x) xs
eq []     = True

applyCouples :: (a -> a -> b) -> [a] -> [b]
applyCouples op xs = [ a `op` b | a:bs <- tails xs, b <- bs ]

anyCouple :: (a -> a -> Bool)  -> [a] -> Bool
anyCouple = or .: applyCouples

