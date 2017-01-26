-- initial variant I've submitted.
import Control.Monad(replicateM, replicateM_)
import Data.List(sortBy, groupBy)

filterElements :: Int -> [Int] -> [Int]
filterElements n = map snd 
  . sortBy (\p1 p2 -> compare (fst p1) (fst p2)) 
  . map head
  . filter (\a -> length a >= n)
  . groupBy (\p1 p2 -> snd p1 == snd p2)
  . sortBy (\p1 p2 -> compare (snd p1) (snd p2)) 
  . zip [1..] 

processTest :: IO [Int]
processTest = do 
  line <- getLine
  let [size, bound] = map read $ words line
  astring <- getLine
  let array = take size $ words astring
  return $ case filterElements bound (map read array) of
            [] -> [-1]
            xs -> xs
    
main :: IO ()
main = do
  n <- getLine
  res <- replicateM (read n) processTest
  mapM_ putStrLn $ map unwords $ map (\line -> map show line) res  
   