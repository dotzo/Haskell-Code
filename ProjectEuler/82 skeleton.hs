import Data.Array

test = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],
        [537,699,497,121,956],[805,732,524,37,331]]

-- Indexes the matrix, change the bounds
indexed = concat $ zipWith (\a l -> map (\(i,x) -> ((a,i),x)) l) [1..5] $ map (zip [1..5]) test

-- Grabs the respective "triangle" of the matrix
topLeft = snd . unzip $ filter (\((x,y),_) -> 5 - y >= x) indexed
botmRight = snd . unzip $ filter (\((x,y),_) -> 5 - y < x) indexed

-- A function to turn the triangles into actual triangles, for use with the 
-- functions already written for problem 67
-- Be sure to reverse the botmRight before applying this
getTri [] _ = []
getTri l i = take i l : getTri (drop i l) (i+1)

-- Get the results of the two triangles using the same fucntion from problem 67
-- and the solution is the sum of the two