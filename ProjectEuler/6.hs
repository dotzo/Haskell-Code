range = [1..100]

answer = abs $ (sum . map (^2) $ range) - (sum range)^2