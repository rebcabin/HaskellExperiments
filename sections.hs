import Debug.Trace
import Data.Set
import Data.List

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

lostNumbers = [4, 8, 15, 16, 23, 42]

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- primes = [2, 3] ++ ([5, 7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes])
