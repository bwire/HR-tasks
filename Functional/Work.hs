readPoint :: String -> (Int, Int)
readPoint =  (\[x, y] -> (read x, read y)) . words
