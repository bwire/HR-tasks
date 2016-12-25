-- Compute the Area of a Polygon 

-- You are given the cartesian coordinates of a set of points in a  plane. 
-- When traversed sequentially, these points form a Polygon, , which is not self-intersecting in nature. Can you compute the area of polygon ?

import Control.Monad(replicateM)
import Control.Applicative((<$>))

main :: IO ()
main = do
  ptsNum <- readLn
  points <- replicateM ptsNum (readPoint <$> getLine)
  print $ abs . (/2) $ foldl (\acc ((x1, y1), (x2, y2)) -> acc + x1 * y2 - y1 * x2) 0 (zip points (tail points ++ [head points]))
  
readPoint :: String -> (Double, Double)
readPoint = (\[x, y] -> (read x, read y)) . words

  



  

  
