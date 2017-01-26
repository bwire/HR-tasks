-- Then, walking through other people's submissions I realized a good idea of using
-- 'on' function from Data.Function. It makes the code more succint. Another amendment is to use
-- function composition while filtering. So I got rid of all lambdas in filterElemens.
 
import Control.Monad(replicateM, replicateM_)
import Data.List(sortBy, groupBy)
import Data.Function(on)

filterElements :: Int -> [Int] -> [Int]
filterElements n = map snd 
  . sortBy (compare `on` fst) 
  . map head
  . filter ((>= n) . length)
  . groupBy ((==) `on` snd)
  . sortBy (compare `on` snd) 
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