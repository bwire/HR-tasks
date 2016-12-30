-- Pascal's triangle

-- For a given integer , print the first  rows of Pascal's Triangle. 
-- Print each row with each value separated by a single space. 
-- The value at the  row and  column of the triangle is equal to  where indexing starts from . These values are the binomial coefficients.
import Control.Monad(mapM_)

main :: IO ()
main = do
  n <- return . read =<< getLine 
  mapM_ putStrLn $ map unwords $ map (\ints -> map show ints) $ pascalTriangle n
      
pascalTriangle :: Int -> [[Int]]
pascalTriangle n = map (\r -> [facts!!(r - 1) `div` (facts!!(c - 1) * facts!!(r - c)) | c <- [1..r]]) [1..n] where
  facts = map (\n -> foldr (*) 1 [1..n]) [0..n]