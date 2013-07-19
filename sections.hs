import Debug.Trace
import Data.Set
import Data.List

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

lostNumbers = [4, 8, 15, 16, 23, 42]

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- primes = [2, 3] ++ ([5, 7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes])

notDvbl num dom = 0 /= mod num dom

-- primes = [2, 3] ++ [ cand | cand <- [5, 7..], 
--                             all (\p -> notDvbl cand p)
--                                 [ p | p <- primes, p*p < cand] ]

primes = [2, 3] ++ [ cand | cand <- [5, 7..], 
                            all (\p -> notDvbl cand p)
                                (takeWhile (\p -> p*p < cand) primes) ]
