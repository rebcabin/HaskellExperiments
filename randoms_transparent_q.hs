import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 $ randomRs ('a','z') gen
  newStdGen
  gen <- getStdGen
  putStrLn $ take 20 $ randomRs ('a','z') gen
