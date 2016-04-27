nthFib :: Int -> Int
nthFib 0 = 1
nthFib 1 = 1
nthFib n = nthFib (n-1) + nthFib (n-2)

genFibs :: [Int] -> [Int]
genFibs [x,y] = x+y : genFibs [y,x+y] 
genFibs _ = error "Provide 2 starting seeds."

fibonacci = 1 : 1 : genFibs [1,1]

answer = sum (takeWhile (<= upper) (map nthFib [3*x - 1 | x <- [1..]]))
    where upper = 4000000