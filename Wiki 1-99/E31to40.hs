import Data.List

-- PROBLEM 31
-- Determine whether a given integer is prime
isPrime :: Int -> Bool
isPrime p = all (/= 0) . map (\x -> p `mod` x) $ 2:[3,5..top]
    where
        top = (p `div` 2) + 1


-- PROBLEM 32
-- Determine the greatest common divisor of two positive interger numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD x 0 = x
myGCD a b 
    | a < b = myGCD b a
    | otherwise = myGCD b (a `mod` b)


-- PROBLEM 33
-- Determine whether two positive integer numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = (== 1) $ myGCD a b


-- PROBLEM 34
-- Calculate Euler's totient function phi(m), the number of positive intgers
-- 1 <= r < m that are coprime to m
totient :: Int -> Int
totient 1 = 1
totient m = sum . filter (== 1) . map (myGCD m) $ [1..(m-1)]


-- PROBLEM 35
-- Determine the prime factors of a given positive integer.  
-- Construct a flat list containing the prime factors in ascending order
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p)
    where
        p = head $ filter (not . coprime n) range
        range = 2:[3,5..]

-- PROBLEM 36 
-- Determine the prime factors of a given positive integer
-- Construct a list containing the prime factors and their multiplcity
primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map (\l -> (head l, length l)) . group . primeFactors


-- PROBLEM 37
-- Calculate the totient function phi(m) using the identity
-- PROD (p - 1) p^(m-1), where p is a prime factor and m is its multiplicity
totient' :: Int -> Int
totient' = foldr (*) 1 . map (\(p,m) -> (p-1) * p^(m-1)) . primeFactorsMult
