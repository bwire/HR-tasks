-- List Replication

-- Given a list, repeat each element in the list  amount of times. 
-- The input and output portions will be handled automatically by the grader. 
-- You need to write a function with the recommended method signature.

f :: Int -> [Int] -> [Int]
f n = concatMap (\x -> replicate n x)

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
