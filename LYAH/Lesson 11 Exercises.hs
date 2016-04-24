import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show,Eq)

-- Make the list a Functor
instance Functor List where
    fmap f Empty = Empty
    fmap f (Value x xs) = Value (f x) (fmap f xs) 

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
Empty `combineLists` ys = ys
(x `Value` xs) `combineLists` ys = x `Value` (xs `combineLists` ys)

-- Make our list a Monoid
instance Monoid (List a) where
    mempty = Empty
    mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
    pure a = Value a Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Value f fs) <*> l = (fmap f l) `mappend` (fs <*> l)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
threeValueList = l
oneValueList = pure 5 :: List Integer

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

-- Use <$> and <*> on the lists with a binary function

-- Create some lists of binary functions
addSubMult = Value (+) $ Value (-) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists

(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x:xs) ++. ys = x : (xs ++. ys)


l = 1 `Value` (2 `Value` (3 `Value` Empty))
l' = 4 `Value` (5 `Value` (6 `Value` Empty))
l'' = 7 `Value` (8 `Value` (9 `Value` Empty))
u = (+) <$> l
v = (+) <$> l'

