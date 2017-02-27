--The Sums of Powers 
import Control.Applicative

sumOfPowers :: Int -> Int -> Int
sumOfPowers n p = 
  let 
    xs = [ns ^ p | ns <- [1..n], ns ^ p <= n]
    process xs delim = foldl (\a e -> folder a e delim xs) [[]] xs 
    folder acc elem delim xs = 
      let
        nd = delim - elem
        xs' = dropWhile (<=elem) xs
        filtered = filter (<=nd) xs'
      in acc ++ (map (elem:) (process filtered nd))
  in if sum xs < n 
  then 0
  else length . filter ((==n) . sum) $ process xs n 
  
main :: IO ()
main = do
  (num, power) <- (,) <$> getLine <*> getLine
  print (sumOfPowers (read num) (read power))