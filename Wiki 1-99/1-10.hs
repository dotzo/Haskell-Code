-- PROBLEM 1
-- Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast _ = error "The empty list has no last element."


-- PROBLEM 2
-- Find the penultimate element of a list
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "This function requires a list of at least length 2."


-- PROBLEM 3
-- Find the K'th element of a list. The first element is number 1
-- e.g. elementAt [1,2,3] 2 = 2
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "This function uses ordinal numbers for position.  First position is #1."
elementAt [] _ = error "You must provide a nonempty list."
elementAt (x:_) 1 = x
elementAt l@(_:xs) n 
    | n < 0 = error "Negative positions make no sense."
    | n > length l = error "Index too large."
    | otherwise = elementAt xs (n-1)


-- PROBLEM 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (\_ a -> 1 + a) 0


-- PROBLEM 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []


-- PROBLEM 6
-- Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome (init xs)
    | otherwise = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs


-- PROBLEM 7
-- Flatten a nested list structure
flatten :: (Foldable t) => t [a] -> [a]
flatten = foldr1 (++)


-- PROBLEM 8
-- Eliminate consecutive duplicastes of list elements
compress :: (Eq a) => [a] -> [a]
compress l = foldr purge [last l] l
    where 
        purge x acc@(y:_)
            | x == y = acc
            | otherwise = x : acc


-- PROBLEM 9
-- Pack consecutive duplicates of list element into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack a@(x:_) = (takeWhile (== x) a) : pack (dropWhile (== x) a)


-- PROBLEM 10
-- Run-length encoding of a list
encode :: (Eq a) => [a] -> [(Int, a)]
encode l = zip num element
    where
        num = map length . pack $ l
        element = map head . pack $ l

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map (\xs -> (length xs, head xs)) . pack