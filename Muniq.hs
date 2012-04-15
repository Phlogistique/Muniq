module Muniq where

import Control.Monad (liftM)

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
splitWhen f (x:xs) = if f x
                   then splitWhen f xs
                   else let (a,b) = splitWhen' f (x:xs)
                        in a : splitWhen f b
splitWhen' f [] = ([],[])
splitWhen' f (x:xs) = if f x
                    then ([],x:xs) 
                    else let (a,b) = splitWhen' f xs
                         in (x:a,b)
isNewline '\n' = True
isNewline '\r' = True
isNewline _    = False

getLines = liftM (splitWhen isNewline) getContents
main = do lines <- getLines
          let Just u = applyPatterns (efficientPatternsFlat $ findPatternsL lines) lines
          putStr $ showULines u

