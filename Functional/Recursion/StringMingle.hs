-- String mingling

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn $ mingle s1 s2

mingle :: String -> String -> String
mingle s1 s2 = concat $ zipWith (:) s1 (map (:[]) s2)