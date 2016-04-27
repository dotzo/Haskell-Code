module UsefulFunctions where

import Data.List
-- Infinite Fibonacci Sequence generator
fibonacci :: (Num a) => [a]
fibonacci = 0 : 1 : genFib [0,1]
    where
        genFib [x,y] = (x+y) : genFib [y,x+y]
        


-- Generates an infinite list of primes using the Sieve of Erasthones
primesGen :: [Int]
primesGen = 2 : sieve [3,5..]
    where
        sieve (x:xs) = x : sieve [d | d <- xs, d `mod` x /= 0]


-- Lists the prime factors of a number in ascending order
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p)
    where
        p = head $ filter (not . coprime n) range
        range = 2:[3,5..]


-- Lists the prime factors of a number along with its multiplicity in the form (prime,multiplicity)
primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map (\l -> (head l, length l)) . group . primeFactors


-- Determines if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = (gcd a b) == 1


-- Determines if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome st = st == reverse st