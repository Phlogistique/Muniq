
data Uniqed a = Single a
              | Group Int [Uniqed a]
  deriving (Show,Eq)

umap :: (Uniqed a -> Uniqed a) -> Uniqed a -> Uniqed a
umap f (Single a) = f $ Single a
umap f (Group n a) = f $ Group n $ map (umap f) a

muniqNoop :: (Eq a) => [a] -> [Uniqed a]
muniqNoop = map Single

muniqFlatten1 :: [Uniqed a] -> [Uniqed a]
muniqFlatten1 = map $ umap muniqFlatten1'
  where
    muniqFlatten1' :: Uniqed a -> Uniqed a
    muniqFlatten1' (Group 1 [Single x]) = Single x
    muniqFlatten1' a = a

muniqFlatten = concatMap muniqFlatten'
  where
    muniqFlatten' (Group 1 x) = concatMap muniqFlatten' x
    muniqFlatten' (Group n x) = [Group n $ concatMap muniqFlatten' x]
    muniqFlatten' (Single x) = [Single x]

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

nuniq :: Eq a => Int -> [a] -> [Uniqed a]
nuniq n = nuniq' n . muniqNoop

slices :: Int -> [a] -> [[a]]
slices n l = take n l : slices n (drop n l)

--nuniq' n l = uniqAndDeSlice n $ slices n l
--  where
--    uniqAndDeSlice :: (Eq a) => Int -> [[Uniqed a]] -> [Uniqed a]
--    uniqAndDeSlice n (x:xs) = uniqAndDeSlice' n xs [] (Group 1 x)
--    uniqAndDeSlice' n (x:xs) acc (Group m y) | x == y    = uniqAndDeSlice' n xs acc $ Group (m+1) y
--                                             | otherwise = uniqAndDeslice' n xs (Group m y : acc) $ Group 1 x
--    uniqAndDeslice' n [] acc g = 
--    uniqAndDeSlice n [] = []
--    uniqAndDeSlice n xs = nuniq' n $ tail $ concat xs

data Pattern = Pattern { patternLength :: Int
                       , patternStart :: Int
                       , patternTimes :: Int
                       }
detectRepeat :: Int -> [a] -> [Pattern]
detectRepeat length list = detectRepeat' length list length
  where
    detectRepeat' length list pos = case startsWithPattern length list pos of
                                      Nothing -> detectRepeat' length (tail list) (pos+1)
                                      Just (rest, pat) -> detectContinue length rest (pos+2*length)
    startsWithPattern :: Int -> [a] -> Maybe ([a], Pattern)
    startsWithPattern length list pos = let start = take length list
                                            rest = drop length list
                                            next = take length rest
                                        in if start == next
                                           then Just (rest, Pattern length pos 2)
                                           else Nothing

    detectContinue

    

