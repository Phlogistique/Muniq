{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, Rank2Types #-}
module Main where

import System.IO          (readFile)
import Control.Monad      (liftM, when)
import Data.List          (lines)
import System.Environment (getArgs, getProgName)

import System.Console.GetOpt

import Muniq
import Muniq.PrettyPrint

getLines = liftM lines getContents

data Options = Options
    { optView :: [Uniqed String] -> String
    , optAlgo :: (Eq a) => [a] -> [Uniqed a]
    , optHelp :: Bool
    }

defaults = Options
    { optView = showULines flat
    , optAlgo = efficientTree
    , optHelp = False
    }

options = [ Option "h" ["help"]
                (NoArg $ \o -> o { optHelp = True })
                "Show help"
          , Option "f" ["flat"]
                (NoArg $ \o -> o { optView = showULines flat })
                "show output with a constant indent"
          , Option "t" ["tree"]
                (NoArg $ \o -> o { optView = showULines indented })
                "show output indented like a tree"
          , Option "" ["et","efficient-tree"]
                (NoArg $ \o -> o { optAlgo = efficientTree })
                "(default) a reasonably fast algorithm for folding short texts"
          , Option "" ["ef","efficient-flat"]
                (NoArg $ \o -> o { optAlgo = efficientFlat })
                "Same as --efficient-tree, with only 1 level of depth"
          , Option "" ["big","big-first"]
                (NoArg $ \o -> o { optAlgo = bigFirst })
                "Always use the longest patterns"
          , Option "" ["small","small-first"]
                (NoArg $ \o -> o { optAlgo = smallFirst })
                "Always use the shortest patterns"
          , Option "" ["bf","brute-force"]
                (NoArg $ \o -> o { optAlgo = bruteForce })
                "Always output the shortest solution (VERY slow)"
          , Option "" ["noop"]
                (NoArg $ \o -> o { optAlgo = noop })
                "Does nothing"
          ]

main = do argv <- getArgs
          name <- getProgName
          let header = "Usage: " ++ name ++ " [options]"
          case getOpt Permute options argv of
            (_,_,errs@(_:_)) -> ioError $ userError $ concat errs ++ usageInfo header options
            (o,n,[]) -> do
                let opts = foldl (flip id) defaults o
                c <- getContents
                m <- mapM readFile n
                let l = concat m 
                
                when (optHelp opts) $ fail "I hate GetOpt"
                putStr $ optView opts $ optAlgo opts $ lines $ c ++ l

