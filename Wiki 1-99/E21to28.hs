import System.Random
import Control.Monad (filterM)
import E11to20 (remove_at)
import Data.List 


-- PROBLEM 21
-- Insert an element at a given position into the list
insert_at :: a -> [a] -> Int -> [a]
insert_at x xs k = front ++ (x:back)
    where
        (front, back) = splitAt (k-1) xs


-- PROBLEM 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range a b = take (b - a + 1) [a..]


-- PROBLEM 23
-- Extract a given number of randomly selected elements from a list
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select _ 0 = return []
rnd_select xs k 
    | 0 < k && k <= length xs = do
        g <- newStdGen
        
        let (r,_) = randomR (1,l) g
            (Just d, ys) = remove_at r xs
            l = (length xs)
        
        recurse <- rnd_select ys (k-1)
        return (d : recurse)
    | otherwise = fail "k out of range [1,length], inclusive"
    

-- PROBLEM 24
-- Lotto: Draw N different random numbers from the set 1..M
lotto :: Int -> Int -> IO [Int]
lotto n m = rnd_select [1..m] n


-- PROBLEM 25
-- Generate a random permutation of the elemnts of a list
rnd_permu :: [a] -> IO [a]
rnd_permu l = rnd_select l (length l)


-- PROBLEM 26
-- Generate the combinations of K distinct objects chosen from the n elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations k l = filter (\xs -> length xs == k) . filterM (\_ -> [True,False]) $ l


-- Problem 27
-- group the elements of a set into disjoint subsets
groups :: (Eq a) => [Int] -> [a] -> [[[a]]]
groups [n] l = [[l]]
groups n'@(n:ns) xs 
    | sum n' /= length xs = error "the total of all the elements in the groups combined must be the same as the total"
    | otherwise = concatMap (replaceLast) step
    where
        step = map (\z -> z : [xs \\ z]) $ combinations n xs
        replaceLast l = map ((init l) ++) (groups ns . last $ l)

-- as a consequence of this if you call length with the group sizes and a range
multinomial :: [Int] -> [Int] -> Int
multinomial n l= length $ groups n l


-- PROBLEM 28a
-- Sort a list of lists by the lengths of the sub-lists
lsort :: (Ord a) => [[a]] -> [[a]]
lsort = map snd . sort . map (\l -> (length l, l))

-- PROBLEM 28b
-- Sort a list of lists by the frequency of their lengths'
lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort = map snd . concat . lsort . groupBy grouping . sort . map (\l -> (length l, l))
    where 
        grouping (x1,_) (y1,_) = x1 == y1
    