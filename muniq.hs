-- This program converts a list of items into a tree exhibiting patterns of
-- repetitions into the list. The names comes from the 'uniq' utility from
-- UNIX. The M is for "multi-line".
--
-- For example, the list
--  1 2 1 2 4 1 2 1 2 4 5
-- could become
--  2×[ 2×[ 1 2 ] 4 ] 5
--
-- However, doing this is non-trivial, as there may be many ways to combine
-- patterns to describe the same list. The program shall implement several
-- strategies:
-- 
-- * Noop: does not detect any pattern
-- * Big-to-small: Always use the longest patterns
-- * Small-to-big: Always use the smallest patterns
-- * Efficiency-first: Always use the patterns that result in the list being
--   the most shortened.
-- * Brute-force: Find the shortest description, no matter the cost.

data Uniqed a = Single a
              | Group Int [Uniqed a]
  deriving (Show,Eq)

-- The above exemple is encoded as:
--  [Group 2 [Group 2 [Single 1, Single 2], Single 4], Single 5]

-- Applies a function to every node of an [Uniqed] tree
mumap = map . umap

umap :: (Uniqed a -> Uniqed a) -> Uniqed a -> Uniqed a
umap f (Single a) = f $ Single a
umap f (Group n a) = f $ Group n $ mumap f a

-- Implements the Noop strategy
muniqNoop :: (Eq a) => [a] -> [Uniqed a]
muniqNoop = map Single

-- Remove redonding patterns. For example, transforms
-- 2×[ 2× [ 1 2 3 4 ] into 4×[ 1 2 3 4 ]
muniqFlatten = concatMap muniqFlatten'
  where
    muniqFlatten' (Group 1 x) = concatMap muniqFlatten' x
    muniqFlatten' (Group n x) = [Group n $ concatMap muniqFlatten' x]
    muniqFlatten' (Single x) = [Single x]

-- Flattens a [Uniqed] tree; can be used to check correctness.
qinum :: [Uniqed a] -> [a]
qinum = concatMap qinum' 
  where
    qinum' (Single a) = [a]
    qinum' (Group n u) = times n $ qinum u

times :: Int -> [a] -> [a]
times n x = concat (replicate n x)

uniq :: Eq a => [a] -> [Uniqed a]
uniq = uniq' . muniqNoop

uniq' (x:y:xs) | x == y = uniq' (Group 2 [x] : xs)
uniq' (Group n [x]:y:xs) | x == y = uniq' (Group (n+1) [x] : xs)
uniq' (x:xs) = x : uniq' xs
uniq' [] = []

data Pattern = Pattern { patternLength :: Int
                       , patternStart :: Int
                       , patternTimes :: Int
                       }
  deriving Show

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n l = let (start, rest) = splitAt n l in start : slices n rest

rle :: Eq a => [a] -> [Int]
rle = rle' 1
  where rle' :: Eq a => Int -> [a] -> [Int]
        rle' n (x:y:xs) | x == y    = rle' (n+1) (y:xs)
                        | otherwise = n : rle' 1 (y:xs)
        rle' n [_] = [n]
        rle' _ []  = []

detectRepeat :: Eq a => Int -> [a] -> [Pattern]
detectRepeat length list = [ Pattern length (offset+length*s) t
                           | offset <- [0 .. (length - 1)]
                           , (t, s) <- let offseted = drop offset list
                                           sliced   = slices length offseted
                                           rle'd    = rle sliced 
                                       in zip rle'd $ scanl (+) 0 rle'd ]

score :: Pattern -> Int
score (Pattern l s t) = (t - 1) * l 

intersect :: Pattern -> Pattern -> Bool
intersect p@(Pattern l s t) p'@(Pattern l' s' t') = let e = endPat p
                                                        e' = endPat p'
                                                    in s <= s' && s' < e || s <= e' && e' < e
segmentIntersect (a,b) (c,d) = b <= c || d <= a

ceilMul :: Integral a => a -> a -> a
ceilMul a b = a + let r = a `mod` b
                  in case r of 0 -> 0
                               _ -> b - r 

endPat (Pattern l s t) = s + l * t

cutPatternAt :: Pattern -> Int -> [Pattern]
cutPatternAt p@(Pattern l s t) c = let e = endPat p 
                                   in if c < s || c >= e
                                      then [p]
                                      else let firstPart  = c - s
                                               firstT     = firstPart `div` l
                                               secondS    = s + ceilMul firstPart l
                                               secondPart = e - secondS
                                               secondT    = secondPart `div` l
                                           in [Pattern l s firstT, Pattern l secondS secondT]

cutPattern :: Pattern -> Pattern -> [Pattern]
cutPattern p@(Pattern l s t) p' = [ p''' | p'' <- cutPatternAt p' s
                                         , p''' <- cutPatternAt p'' (endPat p) ] 

applyCouples :: op -> [a] -> [res]
applyCouples op xs = [ a `op` b | a:bs <- tails xs, b <- bs ]

anyCouple op xs = or (applyCouples op xs)

foldWithPatterns :: [a] -> [Pattern] -> Maybe [Uniqed]
foldWithPatterns l pat | any (\x -> endPat x > length l) pat = Nothing
foldWithPatterns _ pat | anyCouple intersect pat = Nothing
foldWithPatterns l pat = foldWithPatternsUnsafe l pat
                        
foldWithPatternsUnsafe :: [a] -> [Pattern] -> [Uniqed]
foldWithPatternsUnsafe l p = foldl' (flip applyPattern) (muniqNoop l) p

applyPattern :: Pattern -> [Uniqed a] -> [Uniqed a]
applyPattern (Pattern l s t) (x:xs) | s == 0    = Group t (takeU l xs) : dropU (l * t) xs
                                    | otherwise = applyPattern 


efficientFirst :: [a] -> [Pattern]
