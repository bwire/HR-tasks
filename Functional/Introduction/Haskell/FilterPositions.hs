f :: [Int] -> [Int]
f = map snd . filter (even . fst) . (zip [1..])

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
