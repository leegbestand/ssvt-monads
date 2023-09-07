module Main (main) where

import Test.QuickCheck

coords :: Int -> [[(Int, Int)]]
coords n = undefined

-- Will be explained around slide 20.
coordsM n = [1..n] >>= \c1 -> [1..n] >>= \c2 -> return (c1, c2)

-- return for []
lreturn :: a -> [a]
lreturn x = undefined

-- (>>=) for []
lbind :: [a] -> (a -> [b]) -> [b]
lbind comp f = undefined

data Arith = Value Int
           | Add Arith Arith
           | Div Arith Arith
           deriving Show

exampleExpr = Div (Add (Value 1) (Value 2)) (Value 3)
exampleExprFail = Div (Add (Value 1) (Value 2)) (Value 0)


eval :: Arith -> Maybe Int
eval (Value v) = Just v
eval (Add e1 e2) = undefined
eval (Div e1 e2) = undefined


-- return for Maybe
mreturn :: a -> Maybe a
mreturn x = undefined

-- (>>=) for Maybe
mbind :: Maybe a -> (a -> Maybe b) -> Maybe b
mbind comp f = undefined

-- monadic variant of eval
evalM :: Arith -> Maybe Int
evalM (Value v) = return v
evalM (Add e1 e2) = undefined
evalM (Div e1 e2) = undefined

-- Monad generic challenges
challenge1 :: Monad m => (a -> b) -> (a -> m b)
challenge1 f = undefined

challenge2 :: Monad m => m a -> m b -> m b
challenge2 c1 c2 = undefined

challenge3 :: Monad m => [m a] -> m [a]
challenge3 l = undefined

challenge4 :: Monad m => (a -> m b) -> [a] -> m [b]
challenge4 f l = undefined


multiplicationTable :: Integer -> [Integer]
multiplicationTable n = do 
    factor <- [1..10]
    return $ n * factor

multiplicationTable' :: Integer -> [Integer]
multiplicationTable' n = [n * factor | factor <- [1..10]]

multiplicationTable'' :: Integer -> [Integer]
multiplicationTable'' n = [n*1..n*10]

prop_moduloIsZero :: (Integer -> [Integer]) -> Integer -> Bool 
prop_moduloIsZero f x = all (\v -> v `mod` x == 0) (f x)

testPropertyWithGen prop gen = quickCheck $ forAll gen $ prop multiplicationTable'

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

prop_moduloIsZero' :: (Integer -> [Integer]) -> Integer -> Bool
prop_moduloIsZero' f x = x /= 0 --> all (\v -> v `mod` x == 0) (f x)

prop_moduloIsZero'' :: (Integer -> [Integer]) -> Integer -> Property
prop_moduloIsZero'' f x = x /= 10 ==> all (\v -> v `mod` x == 0) (f x)

genNonZero :: Gen Integer
genNonZero = arbitrary `suchThat` (/=0)


-- Implement the suchThat function from QuickCheck
mySuchThat :: Gen a -> (a -> Bool) -> Gen a
mySuchThat arb p = undefined

data Tree a = Leaf a | Tree (Tree a) (Tree a)
    deriving (Show, Eq)

-- Implement an arbitrary instance for the Tree datatype
-- There are many different approaches.
instance Arbitary a => Arbitrary (Tree a) where 
    arbitrary = undefined

main :: IO ()
main = undefined

