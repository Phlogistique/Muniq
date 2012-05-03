module Main where

import Control.Monad (liftM)
import Data.List     (sort)

import Muniq.Uniqed
import Muniq.Utils
import Muniq.Search
import Muniq.PrettyPrint

-- some functions come in three versions:
-- * The safe version, with return type Maybe a (example name: f)
-- * The unsafe version, which may fail (example name: fUnsafe)
-- * The unkind version, which may have an incoherent result if preconditions
--   are not met (exemple name: f')


splitWhen _ [] = []
splitWhen f xs = a : splitWhen f b
  where
    (a,b) = splitWhen' f xs
    splitWhen' f [] = ([],[])
    splitWhen' f (x:xs) | f x       = ([],xs) 
                        | otherwise = let (a,b) = splitWhen' f xs
                                      in (x:a,b)
isNewline '\n' = True
isNewline '\r' = True
isNewline _    = False

getLines = liftM (splitWhen isNewline) getContents
main = do lines <- getLines
          let all = sort $ findPatternsL lines
--              eff = efficientPatternsFlat all
--              u = applyPatterns eff lines
              eff = ept all
              u = apt eff lines
--          print all
--          print eff
--          print u

          putStr $ showULines flat u

