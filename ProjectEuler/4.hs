import UsefulFunctions (isPalindrome)

range = [100..999]

answer = maximum [x*y | x <- range, y <- range, isPalindrome . show $ x*y]