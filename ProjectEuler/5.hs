lcmList :: [Int] -> Int
lcmList = foldr lcm 1

answer = lcmList [1..20]