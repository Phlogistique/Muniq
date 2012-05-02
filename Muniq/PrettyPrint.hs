{-# LANGUAGE PatternGuards #-}
module Muniq.PrettyPrint where

-- Functions that display symbol trees in a human-readable way

import Muniq.Uniqed

showULines :: ([Uniqed String] -> [String]) -> [Uniqed String] -> String
showULines f = foldr1 (++) . map (++"\n") . f

indented = concatMap showULine
  where
    showULine (Single s) = [s]
    showULine (Group n us) = [show n ++ "×["] ++ indent (indented us) ++ ["]"]
    indent = map (indentstr++)
    indentstr = "  "

flat = concatMap showULine
  where
    showULine (Single s) = ['\t':s]
    showULine (Group n [Single s]) = [show n ++ "\t" ++ s]
    showULine (Group n us) = [show n ++ "×["] ++ flat us ++ ["]"]

