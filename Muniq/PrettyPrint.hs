module Muniq.PrettyPrint where

-- Functions that display symbol trees in a human-readable way

import Muniq.Uniqed

showULines :: [Uniqed String] -> String
showULines = showULines' "  " 0

showULines' indent depth ((Single s):xs)  = concat (replicate depth indent) ++ s ++ "\n" ++ showULines' indent depth xs
showULines' indent depth ((Group n u):xs) = concat (replicate depth indent) ++ show depth ++ "×[\n"
                                         ++ showULines' indent (depth+1) u
                                         ++ concat (replicate depth indent) ++ "]\n"

