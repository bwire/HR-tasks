-- String Reductions

-- Given a string, , consisting of  lowercase English characters (), 
-- remove all of the characters that occurred previously in the string. 
import Data.List

main :: IO ()
main = do
   line <- getLine
   putStrLn $ nub line