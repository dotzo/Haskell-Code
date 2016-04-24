module E11to20 where 

import Data.List 

data EncodeList a = Single a | Multiple Int a
    deriving (Show,Eq)

-- Problem 11
-- Encode a list such that singles appear separately
encode :: (Eq a) => [a] -> [EncodeList a]
encode = map dat . group
    where
        dat (x:[]) = Single x
        dat a@(x:_) = Multiple (length a) x


-- PROBLEM 12
-- Decode a list as generated in Problem 11
decode :: [EncodeList a] -> [a]
decode = concatMap d
    where
        d (Single x) = [x]
        d (Multiple n x) = replicate n x


-- PROBLEM 13
-- Encode directly without grouping
encodeWithoutGrouping :: (Eq a) => [a] -> [EncodeList a]
encodeWithoutGrouping = map singletons . foldr con []
    where
        con x [] = (Multiple 1 x):[]
        con x acc@((Multiple n y):ys) 
            | x == y = (Multiple (n+1) y):ys
            | otherwise = (Multiple 1 x):acc
        singletons (Multiple 1 x) = Single x
        singletons a = a


-- PROBLEM 14
-- Duplicatte the elements of a list
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])


-- PROBLEM 15
-- Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs


-- PROBLEM 16
-- Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l 0 = l
dropEvery [x] _ = [x]
dropEvery list n = a ++ (dropEvery (t b) n)
    where
        (a,b) = splitAt (n-1) list
        t [] = []
        t l = tail l


-- PROBLEM 17
-- Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a],[a])
split l 0 = ([],l)
split [] n = ([],[])
split (x:xs) n = (x:f, s)
    where
        f = fst $ split xs (n-1)
        s = snd $ split xs (n-1)


-- PROBLEM 18
-- Extract a slice from a list indexed at 1
slice :: [a] -> Int -> Int -> [a]
slice []  _ _ = []
slice l 1 n = take n l
slice (x:xs) n m
    | n > m = error "first index must be less than the second"
    | n < 1 || m < 1 = error "indexes must be greater than 0"
    | otherwise = slice xs (n-1) (m-1)


-- PROBLEM 19
-- Rorate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate l n
    | n < 0 = rotate l ((length l) + n)
    | otherwise = (drop n l) ++ (take n l)


-- PROBME 20
-- Remove the K'th element from a list
remove_at :: Int -> [a] -> (Maybe a,[a])
remove_at 1 (x:xs) = (Just x,xs)
remove_at k l@(x:xs) = if k <= 0 || k > length l
                        then (Nothing,l)
                        else (f,x:s)
    where
        s = snd $ remove_at (k-1) xs
        f = fst $ remove_at (k-1) xs