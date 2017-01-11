-- Functions Or Not

-- Objective 
-- In this problem, we touch upon a basic concept that is fundamental to Functional Programming: identifying a relation which represents a valid function.

-- Task 
-- You are given a set of unique  ordered pairs constituting a relation. The -values form the domain, and the -values form the range to which they map. For each of these relations, identify whether they may possibly represent a valid function or not.

-- Note: You do not have to find the actual function, you just need to determine that the relation may be representative of some valid function.

import Control.Monad(replicateM_, replicateM, mapM_) 
import Data.List(sortBy, groupBy)

main :: IO()
main = do
  strTestCount <- getLine
  let testCount = read strTestCount :: Int
  results <- replicateM testCount performTest
  mapM_ putStrLn results
  
performTest :: IO String
performTest = do
  strTimes <- getLine
  let times = read strTimes :: Int
  pairs <- replicateM times getPair
  let invRes = filter (\e -> length e > 1) $ 
               groupBy (\t1 t2 -> (fst t1) == (fst t2)) $ 
               sortBy (\t1 t2 -> compare (fst t1) (fst t2)) pairs
  if null invRes then return "YES" else return "NO"
   
getPair :: IO (Int, Int)
getPair = do 
  caseLine <- getLine
  return $ (\[a, b] -> (a, b)) $ map (\w -> read w :: Int) (words caseLine)
 