-- As the further step I picked up the idea of using applicative machinery to reduce the amount of code
-- inside processTest and eliminate all 'let's.
 
import Control.Monad(replicateM, replicateM_)
import Data.List(sortBy, groupBy)
import Data.Function(on)
import Control.Applicative((<$>))

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
  [size, bound] <- map read . words <$> getLine
  array <- take size . words <$> getLine
  return $ case filterElements bound (map read array) of
            [] -> [-1]
            xs -> xs
    
main :: IO ()
main = do
  n <- getLine
  res <- replicateM (read n) processTest
  mapM_ putStrLn $ map unwords $ map (\line -> map show line) res  