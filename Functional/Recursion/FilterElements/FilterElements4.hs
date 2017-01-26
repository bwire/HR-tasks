-- And I can get rid of that process test procedure. The worst element left is that ugly last line.
-- It makes sense print every line inside inner do block. So I can pass an array of Strings to filterElements
-- instead of array of Ints.
-- This final version looks much better!!
-- Thanks to https://codepair.hackerrank.com/paper/DQ8hnENP?b=eyJyb2xlIjoiY2FuZGlkYXRlIiwibmFtZSI6ImJhcmJlZHdpcmUiLCJlbWFpbCI6ImJhcmJlZHdpcmVAdHV0LmJ5In0%3D for the ideas!!

import Control.Monad(replicateM_)
import Data.List(sortBy, groupBy)
import Data.Function(on)
import Control.Applicative((<$>))

filterElements :: Int -> [String] -> [String]
filterElements n = map snd 
  . sortBy (compare `on` fst) 
  . map head
  . filter ((>= n) . length)
  . groupBy ((==) `on` snd)
  . sortBy (compare `on` snd) 
  . zip [1..] 
    
main :: IO ()
main = do
  n <- getLine
  replicateM_ (read n) $ do 
    [size, bound] <- map read . words <$> getLine
    array <- take size . words <$> getLine
    case filterElements bound array of
        [] -> putStrLn "-1"
        xs -> putStrLn $ unwords xs