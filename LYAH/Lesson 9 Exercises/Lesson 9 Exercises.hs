import System.Random
import System.Environment
import Data.List
{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
{-  
main = do
    args <- getArgs
    case head args of
        "-l" -> putStr . show . lottery . read . head . tail $ args
        "-n" -> putStr. unwords . tail $ args
        otherwise -> putStr . unwords $ args
-}

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = let   amt = 6
                    range = [1..49]
                in
                    sort $ pare amt range gen
    where
        pare 0 _ _ = []
        pare _ [] _ = []
        pare n r g = (r !! i) : pare (n-1) (delete (r !! i) r) g'
            where
                (i,g') = randomR (0, subtract 1 $ length r) g
    
