
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

data Pattern = Pattern { patternLength :: Int
                       , patternStart :: Int
                       , patternTimes :: Int
                       }
  deriving Show

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n l = let (start, rest) = splitAt n l in start : slices n rest

detectRepeat :: Eq a => Int -> [a] -> [Pattern]
detectRepeat length list = 
    let detectRepeat' :: Eq a => Int -> [a] -> [Pattern]
        detectRepeat' pos list = [ Pattern length (pos+length*s) t
                                 | (t, s) <- let r = rle $ slices length list 
                                             in zip r $ scanl (+) 0 r
                                 ]
    in [ x | n <- [0 .. (length - 1)]
           , x <- detectRepeat' n $ drop n list
       ]

rle :: Eq a => [a] -> [Int]
rle = rle' 1
  where rle' :: Eq a => Int -> [a] -> [Int]
        rle' n (x:y:xs) | x == y    = rle' (n+1) (y:xs)
                        | otherwise = n : rle' 1 (y:xs)
        rle' n [_] = [n]
        rle' _ []  = []
