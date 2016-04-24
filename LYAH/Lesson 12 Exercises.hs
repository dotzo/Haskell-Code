{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Success a | Fail String deriving (Show,Read,Eq)

--Make the Validation a Monad
instance Monad Validation where
    return = Success 
    fail _ = Fail "Something went wrong in the Monad"
    (Success x) >>= f = f x
    (Fail x) >>= _ = Fail x

instance Applicative Validation where
    pure = Success 
    (Success f) <*> (Success x) = pure (f x)
    _ <*> (Fail x) = Fail x

instance Functor Validation where
    fmap f (Success x) = Success (f x)
    fmap _ (Fail x) = Fail x


{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a, Show a) => a -> Validation a
positiveCheck x = if x > 0
                    then Success x
                    else Fail $ show x ++ " is negative or zero"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a, Show a)  =>  a -> Validation a
evenCheck x = if x `mod` 2 == 0
                then Success x
                else Fail $ show x ++ " is odd"

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a, Show a) => a -> Validation a
positiveAndEvenCheck x = return x >>= positiveCheck >>= evenCheck