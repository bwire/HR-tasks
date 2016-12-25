-- Compute the Perimeter of a Polygon

-- You are given the cartesian coordinates of a set of points in a  plane. 
-- When traversed sequentially, these points form a Polygon, , which is not self-intersecting in nature. Can you compute the perimeter of polygon?

import Control.Monad(replicateM)

main :: IO()
main = do
  strPoints <- getLine
  let points = read strPoints :: Int
  results <- getLengths points
  putStrLn $ show $ sum results

getLengths :: Int -> IO [Double]
getLengths n = do 
  pairs <- replicateM n getPair
  return (map getLength (getLines pairs))

getLength :: ((Double, Double), (Double, Double)) -> Double  
getLength ((x1, y1), (x2, y2)) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)
  
getPair :: IO (Double, Double)
getPair = do 
  caseLine <- getLine
  return $ (\[a, b] -> (fromIntegral a, fromIntegral b)) $ map (\w -> read w :: Int) (words caseLine)

-- this function should have been written with zip, but this is what I came across with initially
getLines :: [a] -> [(a, a)] 
getLines [] = []
getLines [x] = []
getLines [x, y] = [(x, y)]
getLines (x:xs) = map' (x:xs) x where
  map' (y:ys) x | null ys = [(y, x)]
                | otherwise = (y, head ys) : (map' ys x)



  

  
